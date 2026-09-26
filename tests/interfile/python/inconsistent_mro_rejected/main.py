class A:
    def handle(self, x):
        # ok: inconsistent-mro-rejected
        sink(x)


class B(A):
    pass


class X(A, B):
    pass


def run():
    X().handle(source())
