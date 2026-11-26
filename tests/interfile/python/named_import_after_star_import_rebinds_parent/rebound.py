from helpers import *
from util import Model


class C(Model):
    def run(self):
        self.save(source())
