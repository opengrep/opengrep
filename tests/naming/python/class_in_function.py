Foo = 1


def f():
    class Foo:
        pass

    return Foo()


def g():
    return Foo
