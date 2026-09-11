def f(x):
    return x


def shadowed():
    f = source()
    # ruleid: local_shadows_function_taint
    sink(f(1))


def control():
    f = source()
    f(1)
    # ruleid: local_shadows_function_taint
    sink(f)
