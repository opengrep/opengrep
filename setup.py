# type: ignore
# Used for pre-commit since it expects a setup.py in repo root
# for actual setup.py see cli/setup.py
from setuptools import setup

setup(
    name="semgrep_pre_commit_package",
    version="1.10.1",
    # install_requires=["opengrep==1.2.0"], # Commented out since we're not on pypi.
    packages=[],
)
