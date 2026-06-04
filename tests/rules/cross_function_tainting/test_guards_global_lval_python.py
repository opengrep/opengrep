# Guard on a global-variable (BGlob) ToLval, dispatched through a
# forwarding layer. [tagger] taints the module global [gvar] only when
# its guard parameter [flag] holds. [mid] forwards [flag] to [tagger]
# without repackaging.
#
# Direct call (2-level): the guard is decided at instantiation, so the
# [flag=False] read stays clean.
#
# Chained call (3-level): [tagger]'s global ToLval reaches [mid]'s call
# handler, where [mid] re-records it as its own effect. The guard on that
# re-emitted effect must be [mid]'s rebound [flag]; when it was forced to
# [top] the global was tainted unconditionally and the [flag=False] read
# false-positived.


def source():
    return "taint"


def sink(_x):
    pass


gvar = ""


def tagger(flag, val):
    global gvar
    if flag:
        gvar = val


# ---------- Direct call: guard resolves at the top level ----------

def call_direct_no():
    tagger(False, source())
    # ok: test-guards-global-lval
    sink(gvar)


def call_direct_yes():
    tagger(True, source())
    # ruleid: test-guards-global-lval
    sink(gvar)


# ---------- Chained call: guard rebinds across [mid] ----------

def mid(flag, val):
    tagger(flag, val)


def call_chain_no():
    mid(False, source())
    # ok: test-guards-global-lval
    sink(gvar)


def call_chain_yes():
    mid(True, source())
    # ruleid: test-guards-global-lval
    sink(gvar)
