import importlib.resources
import os
import shutil
import sys
from pathlib import Path
from typing import Optional
from semgrep.constants import IS_WINDOWS

from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


def compute_executable_path(exec_name: str) -> Optional[str]:
    """
    Determine full executable path if full path is needed to run it.

    Return None if no executable found
    """
    if IS_WINDOWS:
        exec_name += ".exe"

    if "__compiled__" in globals():
        base_path = os.path.dirname(__file__)
        path = os.path.join(base_path, "bin", exec_name)
        if os.path.isfile(path):
            return path

    # First, try packaged binaries
    try:
        with importlib.resources.path("semgrep.bin", exec_name) as path:
            if path.is_file():
                return str(path)
    except (FileNotFoundError, ModuleNotFoundError) as e:
        logger.debug(f"Failed to open resource {exec_name}: {e}.")

    # Second, try system binaries in PATH.
    #
    # Environment variables, including PATH, are not inherited by the pytest
    # jobs (at least by default), so this won't work when running pytest
    # tests.
    #
    which_exec = shutil.which(exec_name)
    if which_exec is not None:
        return which_exec

    # Third, look for something in the same dir as the Python interpreter
    relative_path = os.path.join(os.path.dirname(sys.executable), exec_name)
    if os.path.isfile(relative_path):
        return relative_path

    return None


class SemgrepCore:
    # Path to the semgrep-core executable, or None if we could not find either
    # or have not looked yet.
    _SEMGREP_PATH_: Optional[str] = None

    @classmethod
    def executable_path(cls) -> str:
        """
        Return the path to the semgrep stand-alone executable binary,
        *not* the Python module.  This is intended for unusual cases
        that the module plumbing is not set up to handle.

        Raise Exception if the executable is not found.
        """
        # This method does not bother to cache its result since it is
        # expected to not be a part of any critical path.
        ret = compute_executable_path("opengrep-core")
        if ret is None:
            raise Exception("Could not locate opengrep-core binary")
        return ret

    @classmethod
    def path(cls) -> Path:
        """
        Return the path to the semgrep stand-alone program.  Raise Exception if
        not found.
        """
        if cls._SEMGREP_PATH_ is None:
            cls._SEMGREP_PATH_ = cls.executable_path()

        return Path(cls._SEMGREP_PATH_)
