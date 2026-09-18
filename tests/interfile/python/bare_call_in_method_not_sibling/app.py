from helpers import handle


class Holder:
    def helper(self, x):
        # A method needs a receiver. A bare call written inside another
        # method of the same class does not reach this one, so no taint
        # arrives here.
        # ok: bare-call-in-method-not-sibling
        sink(x)

    def run(self):
        helper(source())


def go():
    handle(source())
