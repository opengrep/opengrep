class Base:
    def handle(self, x):
        pass


class Left(Base):
    def handle(self, x):
        super().handle(x)


class Right(Base):
    def handle(self, x):
        # ruleid: super-follows-receiver-mro
        sink(x)


class D(Left, Right):
    pass


def run():
    D().handle(source())
