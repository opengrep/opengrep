def f(a, b, n):
    if n > 0:
        return f(b, "safe", n - 1)
    return "safe"


def calls_f():
    # ok: python_recursion_rounds
    sink(f("x", source(), 3))


def p(a, b, n):
    if n > 0:
        return q(b, "safe", n - 1)
    return "safe"


def q(a, b, n):
    return p(a, b, n)


def calls_p():
    # ok: python_recursion_rounds
    sink(p("x", source(), 3))


def keep(a, n):
    if n > 0:
        return keep(a, n - 1)
    return a


def calls_keep():
    # ruleid: python_recursion_rounds
    sink(keep(source(), 3))
