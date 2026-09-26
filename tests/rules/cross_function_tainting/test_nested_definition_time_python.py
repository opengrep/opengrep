def deco(x):
    return lambda f: f


def decorator_binding(c):
    @deco(y := source())
    def inner():
        return y

    if c:
        f = inner
    else:
        f = decorator_binding
    # ruleid: test-nested-definition-time-python
    sink(f())


def decorator_binding_called():
    @deco(y := source())
    def inner():
        return y

    # ruleid: test-nested-definition-time-python
    sink(inner())


def default_value_binding():
    def inner(a=(y := source())):
        return y

    # ruleid: test-nested-definition-time-python
    sink(inner())


def local_binding():
    y = source()

    def inner():
        return y

    # ruleid: test-nested-definition-time-python
    sink(inner())


def decorator_binding_clean():
    @deco(y := "clean")
    def inner():
        return y

    # ok: test-nested-definition-time-python
    sink(inner())


def default_value_binding_clean():
    def inner(a=(y := "clean")):
        return y

    # ok: test-nested-definition-time-python
    sink(inner())


def shadowed_by_inner_local():
    y = source()

    def inner():
        y = "clean"
        return y

    # ok: test-nested-definition-time-python
    sink(inner())
