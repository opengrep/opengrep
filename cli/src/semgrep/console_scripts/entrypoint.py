#!/usr/bin/env python3
# This file is the Semgrep CLI entry point of the Semgrep pip package,
# the Semgrep HomeBrew package, and the Semgrep Docker container.
#
# In the future we may have different entry points when packaging Semgrep
# with Cargo, Npm, Opam, or even with Docker (ideally the entry point would
# be src/main/Main.ml without any wrapper around once osemgrep is finished).
#
# The main purpose of this small wrapper is to dispatch
# either to the legacy pysemgrep (see the pysemgrep script in this
# directory), or to the new osemgrep (accessible via the semgrep-core binary
# under cli/src/semgrep/bin/ or somewhere in the PATH), or even to
# osemgrep-pro (accessible via the semgrep-core-proprietary binary).
#
# It would be faster and cleaner to have a Bash script instead of a Python
# script here, but actually the overhead of Python here is just 0.015s.
# Moreover, it is sometimes hard from a Bash script to find where is installed
# semgrep-core, but it is simple from Python because you can simply use
# importlib.resources. We could also use 'pip show semgrep' from a Bash script
# to find semgrep-core, but will 'pip' be in the PATH? Should we use 'pip' or
# 'pip3'?
# Again, it is simpler to use a Python script and leverage importlib.resources.
# Another alternative would be to always have semgrep-core in the PATH,
# but when trying to put this binary in cli/bin, setuptools is yelling
# and does not know what to do with it. In the end, it is simpler to use
# a *Python* script when installed via a *Python* package manager (pip).
#
# NOTE: if you modify this file, you will need to `pipenv install --dev`
# if you want to test the change under `pipenv shell`.

# Should be done before requests is imported...
import sys

IS_NUITKA = False

if __name__ == "__main__":
    import multiprocessing
    if getattr(sys, 'frozen', False) and hasattr(sys, '_MEIPASS'):
        # PyInstaller binary
        multiprocessing.freeze_support()

    elif "__compiled__" in globals():
        # Nuitka compiled binary
        IS_NUITKA = True
        multiprocessing.freeze_support()

    else:
        # Normal run (e.g., wheel, pip install, dev mode)
        pass

import importlib.resources
import os
import shutil
import sysconfig
import warnings
import subprocess
import semgrep.main
import semgrep.cli
from semgrep.constants import IS_WINDOWS
# from semgrep import tracing

# alt: you can also add '-W ignore::DeprecationWarning' after the python3 above,
# but setuptools and pip adjust this line when installing semgrep so we need
# to do this instead.

warnings.filterwarnings("ignore", category=DeprecationWarning)

# Add the directory containing this script in the PATH, so the pysemgrep
# script will also be in the PATH.
# Some people don't have semgrep in their PATH and call it instead
# explicitly as in /path/to/somewhere/bin/semgrep, but this means
# that calling pysemgrep from osemgrep would be difficult because
# it would not be in the PATH (we would need to pass its path to osemgrep,
# which seems more complicated).
# nosem: no-env-vars-on-top-level
PATH = os.environ.get("PATH", "")
# nosem: no-env-vars-on-top-level
os.environ["PATH"] = PATH + os.pathsep + sysconfig.get_path("scripts")

PRO_FLAGS = ["--pro", "--pro-languages", "--pro-intrafile"]


class CoreNotFound(Exception):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return self.value


# similar to cli/src/semgrep/semgrep_core.py compute_executable_path()
def find_semgrep_core_path():
    core = "opengrep-core"

    if IS_WINDOWS:
        core += ".exe"

    # First check if IS_NUITKA.
    # Look under semgrep/bin since it's only a data diretory:
    if IS_NUITKA:
        base_path = os.path.dirname(__file__)
        path = os.path.join(base_path, "semgrep", "bin", core)
        if os.path.isfile(path):
            return path

    # First, try the packaged binary.
    try:
        # the use of .path causes a DeprecationWarning hence the
        # filterwarnings above
        with importlib.resources.path("semgrep.bin", core) as path:
            if path.is_file():
                return str(path)
    except (FileNotFoundError, ModuleNotFoundError):
        pass

    # Second, try in PATH. In certain context such as Homebrew
    # (see https://github.com/Homebrew/homebrew-core/blob/master/Formula/semgrep.rb)
    # or Docker (see ../../Dockerfile), we actually copy semgrep-core in
    # /usr/local/bin (or in a bin/ folder in the PATH). In those cases,
    # there is no /.../site-packages/semgrep-xxx/bin/semgrep-core.
    # In those cases, we want to grab semgrep-core from the PATH instead.
    path = shutil.which(core)
    if path is not None:
        return path

    raise CoreNotFound(
        f"Failed to find {core} in PATH or in the opengrep package."
    )


# TODO: we should just do 'execvp("pysemgrep", sys.argv)'
# but this causes some regressions with --test (see PA-2963)
# and autocomplete (see #8359)
# TODO: we should get rid of autocomplete anyway (it's a Python Click
# thing not supported by osemgrep anyway),
# TODO: we should fix --test instead.
# The past investigation of Austin is available in #8360 PR comments
def exec_pysemgrep():
    if getattr(sys, 'frozen', False) and hasattr(sys, '_MEIPASS'):  # PyInstaller
        os.environ["SEMGREP_NEW_CLI_UX"] = f"{int(sys.stdout.isatty())}"

    if IS_NUITKA:
        os.environ["SEMGREP_NEW_CLI_UX"] = f"{int(sys.stdout.isatty())}"

    sys.exit(semgrep.main.main())


# We could have moved the code below in a separate 'osemgrep' file, like
# for 'pysemgrep', but we don't want users to be exposed to another command,
# so it is better to hide it.
# We expose 'pysemgrep' because osemgrep itself might need to fallback to
# pysemgrep and it's better to avoid the possibility of an infinite loop
# by simply using a different program name. Morever, in case of big problems,
# we can always tell users to run pysemgrep instead of semgrep and be sure
# they'll get the old behavior.
def exec_osemgrep():
    # if sys.argv[1] == "ci":
    #     # CI users usually want things to just work. In particular, if they
    #     # are running `semgrep ci --pro` they don't want to have to add an
    #     # extra step to install-semgrep-pro. This wrapper doesn't have a way
    #     # to install semgrep-pro, however, so have them run legacy `semgrep`.
    #     print(
    #         "Since `opengrep ci` was run, defaulting to legacy opengrep",
    #         file=sys.stderr,
    #     )
    #     exec_pysemgrep()
    # else:
    try:
        path = find_semgrep_core_path()
    except CoreNotFound as e:
        print(str(e), file=sys.stderr)
        # fatal error, see src/osemgrep/core/Exit_code.ml
        sys.exit(2)

    # If you call opengrep-core as opengrep-cli, then we get
    # opengrep-cli behavior, see src/main/Main.ml
    sys.argv[0] = "opengrep-cli"

    if IS_WINDOWS:
      cp = subprocess.run(sys.argv, executable=str(path), close_fds=True)
      sys.exit(cp.returncode)
    else:
      # nosem: dangerous-os-exec-tainted-env-args
      os.execvp(str(path), sys.argv)

# Needed for similar reasons as in pysemgrep, but only for the legacy
# flag to work.
def main():

    if getattr(sys, 'frozen', False) and hasattr(sys, '_MEIPASS'):
        # PyInstaller
        os.environ["_OPENGREP_BINARY"] = sys.executable
    
    if IS_NUITKA:
        # Nuitka compiled binary
        os.environ["_OPENGREP_BINARY"] = sys.executable

    # escape hatch for users to pysemgrep in case of problems (they
    # can also call directly 'pysemgrep').
    if "--legacy" in sys.argv:
        sys.argv.remove("--legacy")
        exec_pysemgrep()
    elif "--experimental" in sys.argv:
        exec_osemgrep()
    else:
        # we now default to osemgrep! but this will usually exec
        # back to pysemgrep for most commands (for now)
        # We activate the new CLI UX only when semgrep is invoked directly
        # (and legacy is not specified)
        # and osemgrep needs to fallback on pysemgrep
        os.environ["SEMGREP_NEW_CLI_UX"] = f"{int(sys.stdout.isatty())}"
        exec_osemgrep()


if __name__ == "__main__":
    main()
