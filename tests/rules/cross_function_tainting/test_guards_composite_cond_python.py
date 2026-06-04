# Guard whose branch condition embeds a composite ([x, x]) list literal,
# rebound across a forwarding wrapper.
#
# [handler] returns its tainted argument only when [len([x, x]) == flag]
# (i.e. when flag == 2). The condition folds to a concrete bool when
# [flag] is known, but it contains a Composite node. The strict
# parameter-anchoring used to rebind a guard onto the outer function
# rejected any condition containing a Composite/Cast/RecordOrDict, so
# across [wrapper] the guard was dropped and the taint flowed
# unconditionally.
#
# Direct call: [flag] is concrete at the call, so the guard is evaluated
# (not rebound) and the [flag != 2] case stays clean -- a control proving
# the condition folds.
#
# Forward call: the guard must rebind to [wrapper]'s own [flag]; when the
# composite made rebinding fall back to no guard, the [flag != 2] read
# false-positived.


def source():
    return "taint"


def sink(_x):
    pass


def handler(flag, x):
    if len([x, x]) == flag:
        return x
    return ""


# ---------- Direct call: guard evaluated at the call ----------

def call_direct_no():
    # ok: test-guards-composite-cond
    sink(handler(9, source()))


def call_direct_yes():
    # ruleid: test-guards-composite-cond
    sink(handler(2, source()))


# ---------- Forward call: guard rebinds across the wrapper ----------

def wrapper(flag, x):
    return handler(flag, x)


def call_forward_no():
    # ok: test-guards-composite-cond
    sink(wrapper(9, source()))


def call_forward_yes():
    # ruleid: test-guards-composite-cond
    sink(wrapper(2, source()))
