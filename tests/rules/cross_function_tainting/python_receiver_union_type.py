from typing import Optional, Union


class Impl:
    def produce(self):
        return source()


class Other:
    def produce(self):
        return "constant"


def via_optional(other: Optional[Impl]):
    # ruleid: python_receiver_union_type
    sink(other.produce())


def via_union_operator(other: Impl | None):
    # ruleid: python_receiver_union_type
    sink(other.produce())


def via_union(other: Union[Impl, int]):
    # ruleid: python_receiver_union_type
    sink(other.produce())


def via_optional_other(other: Optional[Other]):
    # ok: python_receiver_union_type
    sink(other.produce())
