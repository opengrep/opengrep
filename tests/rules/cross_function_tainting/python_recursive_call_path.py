def f(a, b, n):
    if n > 0:
        return f(b, "safe", n - 1)
    return "safe"


def calls_f():
    # ok: python_recursive_call_path
    sink(f("x", "y", source()))


def g(a, n):
    if n > 0:
        return g(a, n - 1)
    return n


def calls_g():
    # ruleid: python_recursive_call_path
    sink(g("x", source()))
