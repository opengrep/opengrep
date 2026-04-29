# A callback variable assigned at different offsets of the same
# parameter across the branches of an if/else: each alternative offset
# is preserved at the Join and the engine dispatches to every resolved
# callback at the call site. Earlier the unify of two Arg shapes for
# the same parameter took the longest common prefix of their offset
# paths; for divergent paths this collapsed to the empty offset and
# the dispatcher could not resolve any callback. Now the Arg shape
# carries every alternative offset.

def source(): return "taint"
def sink(_x): pass

# Two-way branch — the reviewer's case. Both handlers must fire.
def my_hof_2way(opts, flag):
    if flag:
        cb = opts["a"]
    else:
        cb = opts["b"]
    return cb(opts["data"])

def handler_2way_a(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_2way_b(x):
    # ruleid: test-callback-paths
    sink(x)

def caller_2way():
    my_hof_2way(
        {"a": handler_2way_a, "b": handler_2way_b, "data": source()},
        True,
    )


# Three-way branch — three distinct paths must all fire.
def my_hof_3way(opts, mode):
    if mode == "x":
        cb = opts["x"]
    elif mode == "y":
        cb = opts["y"]
    else:
        cb = opts["z"]
    return cb(opts["data"])

def handler_3way_x(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_3way_y(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_3way_z(x):
    # ruleid: test-callback-paths
    sink(x)

def caller_3way():
    my_hof_3way(
        {
            "x": handler_3way_x,
            "y": handler_3way_y,
            "z": handler_3way_z,
            "data": source(),
        },
        "x",
    )


# Nested branches — paths of different depths. The outer chooses
# between two sub-trees; the inner picks within each. Four callbacks
# at offsets that LCP would have collapsed entirely.
def my_hof_nested(boss, mode):
    if mode == "team_a":
        team = boss["team_a"]
    else:
        team = boss["team_b"]
    if mode == "leader":
        cb = team["leader"]
    else:
        cb = team["sub"]
    return cb(boss["data"])

def handler_a_leader(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_a_sub(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_b_leader(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_b_sub(x):
    # ruleid: test-callback-paths
    sink(x)

def caller_nested():
    my_hof_nested(
        {
            "team_a": {"leader": handler_a_leader, "sub": handler_a_sub},
            "team_b": {"leader": handler_b_leader, "sub": handler_b_sub},
            "data": source(),
        },
        "team_a",
    )


# Same callback bound in both branches — fires once (sort_uniq dedup).
def my_hof_same(opts, flag):
    if flag:
        cb = opts["a"]
    else:
        cb = opts["a"]
    return cb(opts["data"])

def handler_same(x):
    # ruleid: test-callback-paths
    sink(x)

def caller_same():
    my_hof_same({"a": handler_same, "data": source()}, True)


# Negative: callback that is NOT bound by any branch must not fire,
# even though it lives at a sibling key in opts.
def my_hof_two_of_three(opts, flag):
    if flag:
        cb = opts["a"]
    else:
        cb = opts["b"]
    return cb(opts["data"])

def handler_two_of_three_a(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_two_of_three_b(x):
    # ruleid: test-callback-paths
    sink(x)

def handler_unrelated(x):
    # ok: test-callback-paths
    sink(x)

def caller_two_of_three():
    my_hof_two_of_three(
        {
            "a": handler_two_of_three_a,
            "b": handler_two_of_three_b,
            "c": handler_unrelated,
            "data": source(),
        },
        True,
    )
