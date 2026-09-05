def foo8(ok1, danger, ok2):
    # ok:
    sink(ok1)
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok2)

