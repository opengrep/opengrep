class Optional:
    pass


class Impl:
    def produce(self):
        return source()


class Other:
    def produce(self):
        return "constant"


def via_local_optional(other: Optional[Impl]):
    # ok: python_receiver_local_optional
    sink(other.produce())
