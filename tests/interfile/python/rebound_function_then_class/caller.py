from mod_a import Base


def make(data):
    # ok: rebound-function-then-class
    sink(data)


class make(Base):
    pass


def run():
    make(source())
