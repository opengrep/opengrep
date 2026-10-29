from typing import Optional


class A:
    def run(self):
        return source()


class B:
    def run(self):
        return "safe"


def outer_a(x: Optional[A]):
    def inner(x: Optional[B]):
        # ok: python_receiver_union_nested
        sink(x.run())

    # ruleid: python_receiver_union_nested
    sink(x.run())
    return inner


def outer_b(x: Optional[B]):
    def inner(x: Optional[A]):
        # ruleid: python_receiver_union_nested
        sink(x.run())

    # ok: python_receiver_union_nested
    sink(x.run())
    return inner
