# type: ignore
import os
import platform
import shutil
import sys

import setuptools

SOURCE_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(SOURCE_DIR)
# pad: is this still used? git grep SEMGREP_FORCE_INSTALL does not return anything
SEMGREP_FORCE_INSTALL = "SEMGREP_FORCE_INSTALL" in os.environ
IS_WINDOWS = platform.system() == "Windows"
# See ../scripts/build-wheels.sh, which is called from our GHA workflows.
# This script assumes the presence of a semgrep-core binary copied under
# cli/src/semgrep/bin by the caller (the GHA workflow).
WHEEL_CMD = "bdist_wheel"

if WHEEL_CMD in sys.argv:
    try:
        from wheel.bdist_wheel import bdist_wheel
    except ImportError:
        raise Exception(f"The 'wheel' package is required when running '{WHEEL_CMD}'")

    class BdistWheel(bdist_wheel):
        def finalize_options(self):
            bdist_wheel.finalize_options(self)
            self.root_is_pure = False  # We have platform specific binaries

        def get_tag(self):
            _, _, plat = bdist_wheel.get_tag(self)

            # For more information about python compatibility tags, check out:
            # https://packaging.python.org/en/latest/specifications/platform-compatibility-tags/

            # We support Python 3.9+
            # coupling: if you drop support for some python, you'll probably
            # have to update 'python_requires' at the end of this file
            # and a few workflows as show for example in this PR:
            # https://github.com/semgrep/semgrep-proprietary/pull/2606/files
            # coupling: semgrep.libsonnet default_python_version
            python = "cp39.cp310.cp311.cp312.cp313.py39.py310.py311.py312.py313"

            # We don't require a specific Python ABI
            abi = "none"

            # To prevent potential compatibility issues when mixing glibc and libmusl,
            # PyPI does not accept the default linux_x86_64 and linux_aarch64 platform
            # tags. Instead, package maintainers must explicitly identify if their package
            # supports glibc and/or libmusl. Semgrep-core is statically compiled,
            # so this isn't a concern for us.
            if plat == "linux_aarch64":
                plat = "musllinux_1_0_aarch64.manylinux2014_aarch64"
            elif plat == "linux_x86_64":
                plat = "musllinux_1_0_x86_64.manylinux2014_x86_64"
            return python, abi, plat

    cmdclass = {WHEEL_CMD: BdistWheel}
else:
    cmdclass = {}

try:
    with open(os.path.join(REPO_ROOT, "README.md"), "r", encoding="utf-8") as f:
        long_description = f.read()
except FileNotFoundError:
    long_description = "**SETUP: README NOT FOUND**"


def find_executable(env_name, exec_name):
    # First, check for an environment override
    env_value = os.getenv(env_name)
    if env_value:
        return env_value

    # Second, fallback to any system executable
    which_name = shutil.which(exec_name)
    if which_name is not None:
        return which_name

    raise Exception(
        f"Could not find '{exec_name}' executable, tried '{env_name}' and system '{exec_name}'"
    )


install_requires = [
    # versions must be manually synced:
    # - cli/setup.py lists dependencies
    # - cli/Pipfile lists type hint packages for dev env
    # - .pre-commit-config.yaml's mypy hooks also list type hint packages
    #
    # These specifiers are flexible so semgrep can coexist with other tools.
    # Even though we recommend giving semgrep its own virtualenv
    # (or using the official returntocorp/semgrep Docker image),
    # many users will first try to install it in their project's virtualenv.
    #
    # Flexibility is achieved by, in order of preference:
    # 1. >=x if you know the earliest version that works with Semgrep
    # 2. >=x,<y if you know the earliest version that works with Semgrep,
    #    and know that a later version breaks Semgrep.
    # 3. ~=x.0 if you don't know the earliest version that works with Semgrep
    #
    # Try to go from option 3 to 1 over time as you learn more about the codebase.
    #
    # coupling: if you add a dep here, it would be appreciated if you could add
    # it to the top level flake.nix file as well, in
    # pysemgrep.propagatedBuildInputs
    # NOTE: maybe add here `protobuf` and `jaraco`.
    "attrs>=21.3",
    "boltons~=21.0",
    "click-option-group~=0.5",
    "click~=8.1",
    "colorama~=0.4.0",
    "defusedxml~=0.7.1",
    "exceptiongroup~=1.2.0",
    "glom~=22.1",
    "jsonschema~=4.6",
    "packaging>=21.0",
    "peewee~=3.14",
    "requests~=2.22",
    "rich~=13.5.2",
    "ruamel.yaml>=0.18.5",
    "tomli~=2.0.1",
    "typing-extensions~=4.2",
    "urllib3~=2.0",
    "wcmatch~=8.3",
    "protobuf",
    "jaraco.text"
]


setuptools.setup(
    name="opengrep",
    version="1.8.3",
    author="Semgrep Inc., Opengrep",
    author_email="support@opengrep.com",
    description="Lightweight static analysis for many languages. Find bug variants with patterns that look like source code.",
    cmdclass=cmdclass,
    install_requires=install_requires,
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/opengrep/opengrep",
    # creates a .exe wrapper on windows
    entry_points={
        "console_scripts": [
            "opengrep = semgrep.console_scripts.entrypoint:main",
            "pyopengrep = semgrep.console_scripts.pysemgrep:main",
        ]
    },
    packages=setuptools.find_packages(where="src"),
    package_dir={"": "src"},
    package_data={"semgrep": [os.path.join("bin", "*")]},
    include_package_data=True,
    classifiers=[
        "Environment :: Console",
        "License :: OSI Approved :: GNU Lesser General Public License v2 (LGPLv2)",
        "Operating System :: MacOS",
        "Operating System :: POSIX :: Linux",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Security",
        "Topic :: Software Development :: Quality Assurance",
    ],
    python_requires=">=3.9",
    zip_safe=False,
)
