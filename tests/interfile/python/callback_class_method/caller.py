import apply
from worker import Cls


def go():
    apply.run(Cls.handle, source())
