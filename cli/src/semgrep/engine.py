import multiprocessing
from enum import auto
from enum import Enum
from pathlib import Path
from typing import Optional

from semgrep.semgrep_core import SemgrepCore
from semgrep.semgrep_interfaces import semgrep_output_v1 as out
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


class EngineType(Enum):
    OSS = auto()

    @classmethod
    def decide_engine_type(cls) -> "EngineType":
        return cls.OSS

    @staticmethod
    def get_cpu_count() -> int:
        try:
            return multiprocessing.cpu_count()
        except NotImplementedError:  # on Windows
            return 1

    @property
    def default_jobs(self) -> int:
        # Maxing out number of cores used to 16 if more not requested to not overload on large machines
        return min(16, self.get_cpu_count())

    @property
    def default_max_memory(self) -> int:
        return 0  # unlimited

    @property
    def default_interfile_timeout(self) -> int:
        return 0  # unlimited

    def get_binary_path(self) -> Optional[Path]:
        return SemgrepCore.path()

    def check_if_installed(self) -> bool:
        binary_path = self.get_binary_path()
        return binary_path is not None and binary_path.exists()

    @property
    def has_dataflow_traces(self) -> bool:
        return False

    @property
    def is_pro(self) -> bool:
        return False

    @property
    def is_interfile(self) -> bool:
        return False

    def to_engine_kind(self) -> out.EngineKind:
        return out.EngineKind(out.OSS_())
