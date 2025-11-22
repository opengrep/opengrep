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

    # block id --> op result (Intermediate) --> one or more ops
    op_stack: dict[int, dict[Intermediate, list[Op]]] = defaultdict(
        lambda: defaultdict(list)
    )
    region_id_to_block_ids: dict[int, list[int]] = defaultdict(list)
    block_id_to_block_arg_ids: dict[int, list[int]] = {}
    replacements: dict[int, Union[Intermediate, Param]] = {}
    reindex_map: dict[int, int] = {}
    next_fake_intermediate = 0

    def reindex(idx: int) -> int:
        if idx not in reindex_map:
            reindex_map[idx] = len(reindex_map)
        return reindex_map[idx]

    def mlir_to_functions(op: "TritonIROperation") -> None:
        name: str = op.get_name()
        if name == "builtin.module":
            # this wraps all tt.func ops
            return

        operand_ids: list[int] = [
            reindex(op.get_operand(i).id()) for i in range(op.get_num_operands())
        ]
        result_ids: list[int] = [
            reindex(op.get_result(i).id()) for i in range(op.get_num_results())
        ]

        child_block_ids: list[int] = []
        for i in [op.get_region(i).id() for i in range(op.get_num_regions())]:
            # as the walk is bottom-up, the region_id_to_block_ids[i]
            # must be populated by the time we process the enclosing op
            child_block_ids.extend(region_id_to_block_ids[i])

        parent_block_id = -1
        parent_block = op.get_block()
        if parent_block is not None:
            parent_block_id = parent_block.id()
            if parent_block_id not in block_id_to_block_arg_ids:
                block_id_to_block_arg_ids[parent_block_id] = []
                for i in range(parent_block.get_num_arguments()):
                    block_id_to_block_arg_ids[parent_block_id].append(
                        reindex(parent_block.get_argument(i).id()),
                    )
                # the region info is collected via ops' parent blocks to be
                # used later when the region's encloding op is traversed
                parent_region = parent_block.get_parent()
                if parent_region is not None:
                    region_id_to_block_ids[parent_region.id()].append(parent_block_id)

        nonlocal next_fake_intermediate

        if name == "tt.func":
            # for function ops: gather and inline
            # the ops from all child blocks
            fn_ops = defaultdict(list)
            for child_block_id in child_block_ids:
                for result, block_fn_ops in op_stack.pop(child_block_id).items():
                    for block_fn_op in block_fn_ops:
                        fn_ops[result].append(block_fn_op)

            # replace the corresponding Intermediates in the
            # child op args with the function args (Params)
            for i, idx in enumerate(block_id_to_block_arg_ids[child_block_ids[0]]):
                replacements[idx] = Param(i)

            for fn_op_list in fn_ops.values():
                for fn_op in fn_op_list:
                    for i in range(len(fn_op.args)):
                        arg = fn_op.args[i]
                        seen = set()  # to break cycles
                        # there can be transitive replacements, but likely
                        # no cycles (we keep the `seen` set just in case)
                        while (
                            isinstance(arg, Intermediate)
                            and arg.idx in replacements
                            and arg.idx not in seen
                        ):
                            seen.add(arg.idx)
                            arg = fn_op.args[i] = replacements[arg.idx]

            # next function capture starts
            # with empty replacements
            replacements.clear()

            fn_name = op.get_str_attr("sym_name")
            functions[fn_name] = fn_ops
        elif child_block_ids:
            if name in {"scf.if", "scf.for", "scf.while", "tt.reduce", "tt.scan"}:
                # for blocked ops: inline the enclosed ops into
                # the parent block + rewire the last op in each
                # child block to return the block result
                return_ops = []
                for block_id in child_block_ids:
                    if name == "scf.for":
                        # example:
                        # %result = scf.for %iv = %lb to %ub step %step iter_args(%arg = %init) -> (i32) ...
                        # block args: 2 (%iv, %arg)
                        # op operands: 4 (%lb, %ub, %step, %init)
                        # `%arg` is mapping to `%init`
                        for i, idx in enumerate(block_id_to_block_arg_ids[block_id]):
                            if i == 0:
                                next_fake_intermediate -= 1
                                replacements[idx] = Intermediate(next_fake_intermediate)
                            else:
                                replacements[idx] = Intermediate(operand_ids[i + 2])
                    elif name == "scf.while":
                        # example:
                        # %3:3 = scf.while (%arg2 = %1, %arg3 = %2, %arg4 = %c0_i32_8) ...
                        # block args: 3 (%arg2, %arg3, %arg4)
                        # op operands: 3 (%1, %2, %c0_i32_8)
                        # `%arg2` is mapping to `%1`, `%arg3` is mapping to `%2`, ...
                        for i, idx in enumerate(block_id_to_block_arg_ids[block_id]):
                            replacements[idx] = Intermediate(operand_ids[i])
                    elif name == "scf.if":
                        # the scf block args are ignored by the pass. but, as they
                        # may be used as operands of the ops inside the block
                        # (and nested blocks inlined in the current block by now),
                        # they are replaced by new fake Intermediates to avoid "this
                        # operand is not returned by any other op in the fn" error
                        # in the downstream analysis
                        for idx in block_id_to_block_arg_ids[block_id]:
                            next_fake_intermediate -= 1
                            replacements[idx] = Intermediate(next_fake_intermediate)
                    else:
                        assert name in ("tt.reduce", "tt.scan")
                        # wire the block arguments to the op arguments
                        num_operands = len(operand_ids)
                        block_arg_ids = block_id_to_block_arg_ids[block_id]
                        assert len(block_arg_ids) == 2 * num_operands, (
                            f"{name} is expected to have twice as "
                            "many block arguments as op arguments: "
                            f"{operand_ids=}, {block_arg_ids=}."
                        )
                        for i, idx in enumerate(block_arg_ids):
                            # for a tt.reduce/tt.scan op with N arguments, the block
                            # arguments comprise N reduced values followed by
                            # N current values corresponding to the N op args
                            replacements[idx] = Intermediate(
                                operand_ids[i % num_operands]
                            )

                    if block_id in op_stack:
                        block_ops = op_stack.pop(block_id)
                        if not block_ops:
                            continue
                        last_ret, last_ops = block_ops.popitem()
                        if all(
                            op.name
                            in ("scf.yield", "tt.reduce.return", "tt.scan.return")
                            for op in last_ops
                        ):
                            # if last_ops are all return ops, treat them separately
                            return_ops.extend(last_ops)
                        else:
                            # otherwise, return last_ops to the block
                            block_ops[last_ret] = last_ops
                        for op_result, child_ops in block_ops.items():
                            op_stack[parent_block_id][op_result].extend(child_ops)

                scf_results = [Intermediate(idx) for idx in result_ids]

                if return_ops and all(
                    (op.name == "scf.yield" and len(result_ids) == len(op.args))
                    for op in return_ops
                ):
                    # [Note: scf.yield fix-up]
                    #
                    # TL;DR: if our scf.yield takes N args, then we'll create N scf.yield ops to handle each of the
                    # args.
                    #
                    #      **Context**:
                    # During mutation analysis, the analysis pass will identify mutating ops (e.g. tt.store)
                    # and then DFS upwards towards the parameters of the function. Specifically, the analysis pass
                    # looks at the mutated arg in tt.store; then looks for its source ops; and then recurses on the
                    # arguments to each of the source ops.
                    #
                    # In the case of scf.if/scf.for, we may have multiple return ops, each passed as an arg
                    # to scf.yield:
                    #
                    # %18:2 = scf.if %... -> (!tt.ptr<f32>, !tt.ptr<f32>) {
                    #   ...
                    #   scf.yield %1, %2
                    # } else {
                    #   scf.yield %3, %4
                    # }
                    #
                    # And for each of the returns of the scf.if, we'd naively assign the source op of each of the
                    # return values to be the scf.yields. But the scf.yields take _all_ the returns as arguments.
                    # Therefore, if _any_ of the return values of the scf.if are mutated, then the analysis pass
                    # would mark _all_ of the yield args as mutated.
                    #
                    #      **Solution**:
                    # For the purposes of this analysis pass, we create N yield ops - one for each
                    # return-val/yield-arg. In the example above, we'll have two scf.yield's for each branch of the
                    # scf.if.

                    for return_op in return_ops:
                        for i, (scf_result, yield_arg) in enumerate(
                            zip(scf_results, return_op.args)
                        ):
                            sub_yield_op = Op(
                                return_op.name,
                                return_op.fn_call_name,
                                [yield_arg],
                                return_op.ret,
                                sub_idx=i,
                            )
                            op_stack[parent_block_id][scf_result].append(sub_yield_op)

                else:
                    for scf_result in scf_results:
                        for return_op in return_ops:
                            op_stack[parent_block_id][scf_result].append(return_op)
            else:
                raise RuntimeError(
                    f"Unknown blocked function: {name}. Can't capture the TTIR."
                )
        else:
            callee = None
            if name == "tt.call":
                callee = op.get_flat_symbol_ref_attr("callee")
            args: list[Union[Param, Intermediate]] = [
                Intermediate(operand) for operand in operand_ids
            ]
            block_ops = op_stack[parent_block_id]

            is_pure = False
            # Handle the case for tt.elementwise_inline_asm to set `is_pure` for mutation analysis
            if name == "tt.elementwise_inline_asm":
                is_pure = op.get_bool_attr("pure")

            if result_ids:
                for result_id in result_ids:
                    res = Intermediate(result_id)
                    block_ops[res].append(Op(name, callee, args, res, is_pure=is_pure))
            else:
                next_fake_intermediate -= 1
                fake_res = Intermediate(next_fake_intermediate)
                block_ops[fake_res].append(
                    Op(name, callee, args, fake_res, is_pure=is_pure)
                )

    ttir_module.walk(mlir_to_functions)

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

    
