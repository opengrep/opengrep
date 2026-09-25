# A class is a callable value: calling it through a parameter constructs an
# instance and runs the class's __init__.
class Foo:
    def __init__(self, x):
        # ruleid: class_as_callable_value_python
        sink(x)


class Bar:
    def __init__(self, x):
        # ok: class_as_callable_value_python
        sink(x)


def make(k):
    return k(source())


def run():
    make(Foo)
