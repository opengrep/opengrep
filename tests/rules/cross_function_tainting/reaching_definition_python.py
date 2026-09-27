# A def statement rebinds its name when it runs: a call reaches the
# definition that the last def statement executed before it bound.
def f(a):
    # ok: reaching_definition_python
    sink(a)


f("constant")


def f(a):
    # ruleid: reaching_definition_python
    sink(a)


f(source())


def g(a):
    # ruleid: reaching_definition_python
    sink(a)


g(source())


def g(a):
    # ok: reaching_definition_python
    sink(a)


g("constant")
