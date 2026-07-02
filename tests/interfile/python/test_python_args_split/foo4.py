def foo4(danger, ok1, ok2):
    # ruleid: taint
    sink(danger)
    # ok:
    sink(ok1)
    # ok:
    sink(ok2)

