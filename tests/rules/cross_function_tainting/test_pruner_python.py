# Unreachable-branch pruning: a statically dead branch must not produce
# findings, must not detect fresh sources, must not contribute to the
# function's signature, and must not leak its writes past the Join with
# a live branch.

def source(): return "taint"
def sink(_x): pass

# Pre-existing tainted lval, sink in dead branch.
def case_pre_existing():
    a = source()
    if False:
        # ok: test-pruner-python
        sink(a)

# Fresh source produced inside the dead branch.
def case_fresh_in_branch():
    if False:
        # ok: test-pruner-python
        sink(source())

# Fresh source via an intermediate local.
def case_fresh_via_local():
    if False:
        x = source()
        # ok: test-pruner-python
        sink(x)

# Symmetric direction — dead else of `if True`.
def case_else_dead():
    if True:
        x = ""
    else:
        # ok: test-pruner-python
        sink(source())

# Constant-folded condition (length over a known-length literal).
def case_folded_cond():
    arr = [1]
    if len(arr) == 2:
        # ok: test-pruner-python
        sink(source())

# Nested dead branches.
def case_nested_dead():
    if False:
        if False:
            # ok: test-pruner-python
            sink(source())

# Post-Join leakage: dead branch's write must not survive the Join.
def case_post_join_leakage():
    x = "safe"
    if False:
        x = source()
    # ok: test-pruner-python
    sink(x)

# Function whose ToReturn comes only from a dead branch — caller
# must not fire on the result.
def inner_only_dead():
    if False:
        return source()
    return ""

def outer_caller_only_dead():
    # ok: test-pruner-python
    sink(inner_only_dead())

# Live counterpart — sanity check that the analysis still fires for
# code that isn't dead.
def case_live_fires():
    if True:
        # ruleid: test-pruner-python
        sink(source())
