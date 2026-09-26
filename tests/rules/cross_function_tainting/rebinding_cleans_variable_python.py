import sys


def rebound():
    x = {"path": sys.argv[1]}
    x = {"name": sys.argv[2]}
    # ok: rebinding_cleans_variable_python
    open(x["path"])


def kept():
    x = {"path": sys.argv[1]}
    y = {"name": sys.argv[2]}
    # ruleid: rebinding_cleans_variable_python
    open(x["path"])
