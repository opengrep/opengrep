# Guard on a global-variable (BGlob) ToLval, dispatched through a
# forwarding layer.  [tagger] taints the module global [gvar] only when
# its guard parameter [flag] holds.  [mid] (in mid.py) forwards [flag] to
# [tagger] without repackaging.
#
# Python's `global gvar` binds to the *current module's* global namespace,
# so [gvar], its writer [tagger], and every reader below must live in this
# one module.  The interfile aspect is the call graph: the readers reach
# [tagger] directly or through [mid] in another file, and the guard on the
# re-emitted global effect must stay [mid]'s rebound [flag].  [mid] takes
# [tagger] as an argument so it needs no back-import of this module.
from mid import mid
from sink import sink
from source import source

gvar = ""


def tagger(flag, val):
    global gvar
    if flag:
        gvar = val


# ---------- Direct call: guard resolves at the top level ----------
def call_direct_yes():
    tagger(True, source())
    # ruleid: test-guards-global-lval
    sink(gvar)


def call_direct_no():
    tagger(False, source())
    # ok: test-guards-global-lval
    sink(gvar)


# ---------- Chained call: guard rebinds across [mid] ----------
def call_chain_yes():
    mid(tagger, True, source())
    # ruleid: test-guards-global-lval
    sink(gvar)


def call_chain_no():
    mid(tagger, False, source())
    # ok: test-guards-global-lval
    sink(gvar)
