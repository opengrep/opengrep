from helpers import Box, rebind, fill_then_rebind


def rebinding():
    c = "safe"
    rebind(c, source())
    # ok: test-param-rebinding-split
    sink(c)


def filled_before_rebinding():
    b = Box()
    fill_then_rebind(b, source())
    # ruleid: test-param-rebinding-split
    sink(b.f)
