# Guard on a callback's return decided through higher-order dispatch.
# [apply] returns its callback's result; [handler] returns taint only
# when its guard parameter [flag] holds. The guard must be decided at the
# top-level caller: evaluated against a concrete literal, or rebound
# across a forwarding wrapper. (On main, with no guards, every call below
# false-positives.)

def source():
    return "taint"


def sink(_x):
    pass


def apply(cb, flag, x):
    return cb(flag, x)


def handler(flag, x):
    if flag:
        return x
    return ""


# ---------- Guard value supplied as a literal at the call site ----------

def call_literal_no():
    # ok: test-guards-hof-return
    sink(apply(handler, False, source()))


def call_literal_yes():
    # ruleid: test-guards-hof-return
    sink(apply(handler, True, source()))


# ---------- Guard forwarded through a wrapper (rebinding) ----------

def wrapper(flag, x):
    return apply(handler, flag, x)


def call_forward_no():
    # ok: test-guards-hof-return
    sink(wrapper(False, source()))


def call_forward_yes():
    # ruleid: test-guards-hof-return
    sink(wrapper(True, source()))
