# Imports `process` from the package `mypkg`.  The name lives in
# mypkg/processor.py, not in mypkg/__init__.py — __init__.py only
# re-exports it.  Resolution of `process` here depends on projidx's
# __init__.py re-export tracking.
from mypkg import process


def source():
    return input()


def main():
    tainted = source()
    process(tainted)


main()
