# HOF guard rebinding across a call chain: the inner function's
# [ToSinkInCall] effect (the callback invocation) is guarded by a
# branch condition on one of its parameters. Outer forwards both
# the callback and the guard-relevant parameter to inner without
# repackaging. At top-level instantiation the guard should drop
# effects whose top-level-caller argument fails the condition.
#
# Each intermediate frame rebinds the guard to its own parameter
# positions, which are intentionally shifted from the callee's to
# surface any index-arithmetic bug in the rebinding.


def source():
    return "taint"


def sink(_x):
    pass


# ---------- No finding: top-level list has len != 2 ----------

def do_sink_no(v):
    sink(v)


def inner_hof_no(a, cb, b, x):             # cb=1, x=3
    if len(a) == 2:
        cb(x)


def outer_hof_no(c, d, my_cb, e, my_list, f, my_x):  # my_cb=2, my_list=4, my_x=6
    inner_hof_no(my_list, my_cb, "b", my_x)


def call_hof_no():
    outer_hof_no("c", "d", do_sink_no, "e", [1], "f", source())


# ---------- Finding expected: top-level list has len == 2 ----------

def do_sink_yes(v):
    # ruleid: test-guards-rebind-hof
    sink(v)


def inner_hof_yes(a, cb, b, x):
    if len(a) == 2:
        cb(x)


def outer_hof_yes(c, d, my_cb, e, my_list, f, my_x):
    inner_hof_yes(my_list, my_cb, "b", my_x)


def call_hof_yes():
    outer_hof_yes("c", "d", do_sink_yes, "e", [1, 2], "f", source())
