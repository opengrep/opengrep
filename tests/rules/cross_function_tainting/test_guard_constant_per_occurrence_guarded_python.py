def h(p):
    x = 1
    if x == 1:
        y = 0
    x = g()
    if x == 1:
        y = 1
    else:
        # ruleid: test-guard-constant-per-occurrence
        sink(p)


def f():
    h(source())


def k():
    p = source()
    x = 1
    if x == 1:
        y = 0
    x = g()
    if x == 1:
        y = 1
    else:
        # ruleid: test-guard-constant-per-occurrence
        sink(p)
