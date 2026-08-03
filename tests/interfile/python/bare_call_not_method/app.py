from helpers import unrelated


class B:
    def helper(self, x):
        # A method needs a receiver: a bare `helper(...)` call cannot
        # reach it, so no taint should arrive here.
        # ok: bare-call-not-method
        sink(x)


def run():
    # `helper` is not defined in this file (builtin, star-import, or
    # simply unresolvable). It must NOT resolve to B.helper.
    helper(source())
