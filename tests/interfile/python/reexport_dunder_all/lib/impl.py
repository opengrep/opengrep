__all__ = ["_run"]


def _run(data):
    # ruleid: reexport-dunder-all
    sink(data)


def helper(data):
    # `helper` is public but NOT in __all__, so `import *` must not bring
    # it in; a call to it via the package must stay unresolved.
    # ok: reexport-dunder-all
    sink(data)
