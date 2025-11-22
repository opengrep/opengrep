import collections
import copy
import dataclasses
import functools
import inspect
import itertools
import logging
import operator
import threading
from collections import defaultdict
from collections.abc import Sequence
from typing import Any, Callable, Optional, TYPE_CHECKING, Union
from typing_extensions import Never

import sympy

import torch.fx as fx
import torch.utils._pytree as pytree
from torch import SymInt, Tensor
from torch._C import DispatchKey
from torch._higher_order_ops.utils import redirect_to_mode
from torch._ops import HigherOrderOperator
from torch._prims_common import clone_preserve_strides
from torch._subclasses.fake_tensor import FakeTensorMode
from torch.fx.experimental.proxy_tensor import (
    disable_proxy_modes_tracing,
    ProxyTorchDispatchMode,
    track_tensor_tree,
)
from torch.fx.experimental.symbolic_shapes import guard_scalar
from torch.types import IntLikeType
from torch.utils.checkpoint import _CachedTorchDispatchMode, _CachingTorchDispatchMode


if TYPE_CHECKING:
    from triton._C.libtriton.ir import (
        module as TritonIRModule,
        operation as TritonIROperation,
    )

    from torch._dynamo.symbolic_convert import InstructionTranslator
    from torch._dynamo.variables.constant import ConstantVariable
    from torch._dynamo.variables.functions import TritonKernelVariable
    from torch._subclasses.functional_tensor import BaseFunctionalizeAPI
    from torch.fx.proxy import Proxy
    from torch.utils._triton import has_triton

    TritonMetaParamsType = dict[str, int]
    TritonGridTupleType = tuple[Union[int, sympy.Expr, SymInt], ...]
    TritonGridCallableType = Callable[[TritonMetaParamsType], tuple[int, ...]]
    TritonGridType = Union[TritonGridTupleType, TritonGridCallableType]

    if has_triton():
        from triton.runtime.autotuner import Autotuner, Config as TritonConfig
        from triton.runtime.jit import JITFunction
    else:

        class Autotuner:  # type: ignore[no-redef]
            pass

        class JITFunction:  # type: ignore[no-redef]
            pass

    TritonKernelType = Union[Autotuner, JITFunction]
    # mypy specifically complains that TritonAutotunerType is not a valid type if Autotuner is not inside of a Union.
    TritonAutotunerType = Union[Autotuner]

log = logging.getLogger("torch._dynamo")

# e.g. for a host-side Triton TMA API call ``create_2d_tma_descriptor(ptr, 50, 60, 32, 15, 4)``,
# the metadata will look like ``("experimental", ([50, 60], [32, 15], 4))``
TMAExperimentalMetadata = tuple[
    str,  # type of TMA (should be "experimental")
    tuple[
        list[IntLikeType],  # dims
        list[IntLikeType],  # block_dims
        IntLikeType,  # element_size
    ],
]

# e.g. for host-side Triton TMA API call ``TensorDescriptor.from_tensor(ptr, [32, 64])``
# the metadata will look like ``("stable", ([32, 64],))``
TMAStableMetadata = tuple[
    str,  # type of TMA ("experimental" or "stable")
    tuple[list[IntLikeType],],  # block_shape
]


def create_tma_experimental_metadata(
    dims: list[IntLikeType],
    block_dims: list[IntLikeType],
    element_size: IntLikeType,
) -> TMAExperimentalMetadata:
    return ("experimental", (dims, block_dims, element_size))


def maybe_unpack_tma_experimental_metadata(
    tma_meta: Union[TMAExperimentalMetadata, TMAStableMetadata],
) -> Optional[tuple[list[IntLikeType], list[IntLikeType], IntLikeType]]:
    if not tma_meta or len(tma_meta) != 2:
        return None
    if tma_meta[0] == "experimental":
        return tma_meta[1]  # type: ignore[return-value]
    return None


def create_tma_stable_metadata(
    block_shape: list[IntLikeType],
) -> TMAStableMetadata:
    return ("stable", (block_shape,))


def maybe_unpack_tma_stable_metadata(
    tma_meta: Union[TMAExperimentalMetadata, TMAStableMetadata],
) -> Optional[tuple[list[IntLikeType]]]:
    if not tma_meta or len(tma_meta) != 2:
        return None
    if tma_meta[0] == "stable":
        return tma_meta[1]  # type: ignore[return-value]
    return None


# TMADescriptorMetadata maps kernel parameter names to the metadata that allows
# reconstructing TMA descriptors from the underlying tensors (passed as kernel
# arguments in the fx graph, instead of the TMA descriptors).
#
# Since there are two TMA APIs (the old "experimental" API and the new "stable" API),
# each entry in the dict is a tuple that starts with a string, either "experimental"
# or "stable". The second entry in the tuple is another tuple, with data that depends
# on the API type (see TMAExperimentalMetadata and TMAStableMetadata above).
#
# These are stored as raw tuples (instead of classes) for ease of serialization.
TMADescriptorMetadata = dict[
    str,  # kernel parameter name
    Union[TMAExperimentalMetadata, TMAStableMetadata],
]


###############################################################################
# Kernel Side Table


# We cannot put Triton Kernels into the FX graph as the graph nodes
# do not support arbitrary functions.
# Use a side table.
# We use two dicts so that fetching both the kernel and id are O(1)
class KernelSideTable:
    id_to_kernel: dict[int, "TritonKernelType"] = {}
    kernel_to_id: dict["TritonKernelType", int] = {}
    constant_args: dict[int, dict[str, Any]] = {}
    lock = threading.Lock()

    # Returns index on the table
    def add_kernel(self, kernel: "TritonKernelType") -> int:
        with self.lock:
            if kernel in self.kernel_to_id:
                return self.kernel_to_id[kernel]

            idx = len(self.id_to_kernel)
            self.id_to_kernel[idx] = kernel
            self.kernel_to_id[kernel] = idx
            return idx

    # Returns the triton kernel at the given index
    def get_kernel(self, idx: int) -> "TritonKernelType":
        # No need to lock here as fetching from dict is atomic
        assert idx in self.id_to_kernel
        return self.id_to_kernel[idx]

    # Not every constant arg can be added to the graph. Use this side table
    # for constant args.
    def add_constant_args(self, args: dict[str, Any]) -> int:
        with self.lock:
            idx = len(self.constant_args)
            self.constant_args[idx] = args
            return idx

    # Returns the constant args
    def get_constant_args(self, idx: int) -> dict[str, Any]:
        # No need to lock here as fetching from dict is atomic
        assert idx in self.constant_args
        return self.constant_args[idx]

    # Resets the table (only meant to be used in unit tests)
    # This is only safe assuming single threaded execution
    def reset_table(self) -> None:
        self.id_to_kernel = {}
        self.kernel_to_id = {}
        self.constant_args = {}


kernel_side_table = KernelSideTable()


###############################################################################
# Mutation Tracker







def ttir_to_functions(
    ttir_module: "TritonIRModule",
) -> dict[str, dict[Intermediate, list[Op]]]:
    """
    Walk the `ttir_module` bottom up to mine the `functions` from
    the structured MLIR entities representing the Triton kernel
    (mlir::Operation, mlir::Block, mlir::Region).
    """
    functions: dict[str, dict[Intermediate, list[Op]]] = {}

    
    return functions










def triton_kernel_wrapper_mutation_dense(
    *,
    kernel_idx: int,
    constant_args_idx: int,
    grid: list["TritonGridType"],
    tma_descriptor_metadata: TMADescriptorMetadata,
    kwargs: dict[str, Any],
) -> None:
    from torch._inductor.codegen.wrapper import user_defined_kernel_grid_fn_code

    kernel = kernel_side_table.get_kernel(kernel_idx)
    constant_args = kernel_side_table.get_constant_args(constant_args_idx)

    if len(grid) == 1:
        grid_fn = grid[0]
    else:
        fn_name, code = user_defined_kernel_grid_fn_code(
            kernel.fn.__name__, kernel.configs, grid
        )
        namespace: dict[str, Any] = {}
        exec(code, namespace)
        grid_fn = namespace[fn_name]

    
