from lib import *


class Base:
    def f(self, x):
        # ruleid: own-class-parent-ahead-of-star-import
        sink(x)


class C(Base):
    def run(self):
        self.f(source())
