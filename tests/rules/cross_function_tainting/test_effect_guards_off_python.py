# Without the [guarded_taint_signatures] rule option no guard is created: the branch
# condition below is not attached to the sink effect, so the call with
# flag=0 -- which the guard would refute -- still reports. Pins that the
# guard machinery is inert by default.


def source():
    return "taint"


def sink(_x):
    pass


def f(flag, x):
    if flag == 1:
        # ruleid: test-effect-guards-off
        sink(x)


def call():
    f(0, source())
