def extend_in_place(items):
    items += [source()]


def concat(items):
    items = items + [source()]


def fresh(items):
    items = []
    items.append(source())


def extend_string(s: str):
    s += source()


def t1():
    l = []
    extend_in_place(l)
    # ruleid: test-param-rebinding-per-path-python
    sink(l)


def t2():
    l = []
    concat(l)
    # ok: test-param-rebinding-per-path-python
    sink(l)


def t3():
    l = []
    fresh(l)
    # ok: test-param-rebinding-per-path-python
    sink(l)


def t4():
    s = ""
    extend_string(s)
    # ok: test-param-rebinding-per-path-python
    sink(s)
