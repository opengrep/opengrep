class Base:
    def handle(self, x):
        pass


class Right(Base):
    def handle(self, x):
        # ruleid: super-follows-receiver-mro-control
        sink(x)


class Left(Right):
    def handle(self, x):
        super().handle(x)


class D(Left):
    pass


def run():
    D().handle(source())
