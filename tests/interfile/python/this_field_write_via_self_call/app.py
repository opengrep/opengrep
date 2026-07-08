# P2.4 (interfile): [run] taints [self.data] by calling [self.set_it], which
# reaches [run] only as a ToLvalThis effect (the receiver is [run]'s own
# [this]).  The later local read [self.data] must see that taint and carry it
# to the cross-file sink [emit] in base.py — so the finding requires both the
# local read-after-write fix AND interfile resolution of [emit].
from base import emit


class Box:
    def set_it(self, x):
        self.data = x

    def run(self):
        t = source()
        self.set_it(t)
        emit(self.data)
