def pick(c):
    x = source()
    return (lambda: x) if c else (lambda: 0)


def pick_rev(c):
    x = source()
    return (lambda: 0) if c else (lambda: x)


def pick_clean(c):
    x = source()
    y = "clean"
    return (lambda: y) if c else (lambda: 0)


def returned_closures(c):
    # ruleid: test-closure-join-py
    sink(pick(c)())
    # ruleid: test-closure-join-py
    sink(pick_rev(c)())
    # ok: test-closure-join-py
    sink(pick_clean(c)())


def same_parameters(c):
    x = source()
    if c:
        # ruleid: test-closure-join-py
        f = lambda a: sink(a)
    else:
        f = lambda a: print(a)
    f(x)


def same_parameters_rev(c):
    x = source()
    if c:
        f = lambda a: print(a)
    else:
        # ruleid: test-closure-join-py
        f = lambda a: sink(a)
    f(x)


def different_parameters(c):
    x = source()
    if c:
        # ruleid: test-closure-join-py
        f = lambda a, *rest: sink(a)
    else:
        # ruleid: test-closure-join-py
        f = lambda a, b, *rest: sink(b)
    f(x, x)


def apply(cb, v):
    cb(v)


def callback_chosen_by_branch(c):
    x = source()
    if c:
        g = lambda a: print(a)
    else:
        # ruleid: test-closure-join-py
        g = lambda a: sink(a)
    apply(g, x)


def same_lambda_in_loop(xs):
    y = source()
    f = lambda: y
    for _ in xs:
        f = (lambda: y) if xs else f
    # ruleid: test-closure-join-py
    sink(f())
