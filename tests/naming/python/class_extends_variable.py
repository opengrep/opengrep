def f():
    Foo = make()

    class Foo(Foo):
        pass

    return Foo()
