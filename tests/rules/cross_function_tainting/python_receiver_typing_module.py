import typing
import typing as t


class Impl:
    def produce(self):
        return source()


class Other:
    def produce(self):
        return "constant"


def via_typing_optional(other: typing.Optional[Impl]):
    # ruleid: python_receiver_typing_module
    sink(other.produce())


def via_alias_optional(other: t.Optional[Impl]):
    # ruleid: python_receiver_typing_module
    sink(other.produce())


def via_typing_union(other: typing.Union[Impl, None]):
    # ruleid: python_receiver_typing_module
    sink(other.produce())


def via_typing_optional_other(other: typing.Optional[Other]):
    # ok: python_receiver_typing_module
    sink(other.produce())
