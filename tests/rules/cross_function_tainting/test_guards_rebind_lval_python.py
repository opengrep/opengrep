# ToLval guard rebinding across a call chain. [tagger] guards a
# side-effect assignment to [obj.x] on a parameter. A forwarder passes
# its parameters through to [tagger]; a top-level caller then either
# satisfies or violates the guard, which controls whether [obj.x] ends
# up tainted and thus whether reading it downstream produces a finding.
#
# [forwarder_shift] moves [flag] to a different parameter index than
# [tagger] expects, so the guard's parameter anchor must be rebound
# across the hop rather than carried by position.
#
# Uses an explicit [obj] parameter (BArg ToLval) rather than a [self]
# receiver (BThis ToLval). BThis propagation through method calls is a
# pre-existing issue in opengrep unrelated to rebinding.


def source():
    return "taint"


def sink(_x):
    pass


class C:
    pass


def tagger(obj, flag, val):
    if flag:
        obj.x = val


# ---------- Direct call: guard resolves at the top level ----------

def call_direct_no():
    c = C()
    tagger(c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


def call_direct_yes():
    c = C()
    tagger(c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)


# ---------- Chain call (same parameter order) ----------

def forwarder(obj, flag, val):
    tagger(obj, flag, val)


def call_chain_no():
    c = C()
    forwarder(c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


def call_chain_yes():
    c = C()
    forwarder(c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)


# ---------- Chain call with a shifted parameter index ----------
# [flag] is at index 2 here but index 1 in [tagger]; the guard anchor
# must be rebound across the hop.

def forwarder_shift(dummy, obj, flag, val):
    tagger(obj, flag, val)


def call_shift_no():
    c = C()
    forwarder_shift("d", c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


def call_shift_yes():
    c = C()
    forwarder_shift("d", c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)
