from typing import Optional


class Impl:
    def produce(self):
        return source()


class Other:
    def consume(self):
        return "constant"


def via_forward_reference(other: Optional["Impl"]):
    # ruleid: python_receiver_unread_type
    sink(other.produce())


def via_forward_reference_other(other: Optional["Other"]):
    # ok: python_receiver_unread_type
    sink(other.consume())
