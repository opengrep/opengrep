def foo9(ok1, ok2, danger):
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)
    # ruleid: taint
    sink(danger)

