# Disjoint-branch ToReturn through a forwarder. The callee returns a
# tainted value on multiple branches with disjoint, parameter-anchored
# guards. A forwarder calling it must propagate the disjunction so that
# every caller satisfying the precondition fires.
#
# Each branch's [source()] call produces a distinct taint identity, so
# at the call-effect handler the per-taint storage keeps them as
# separate bundles each carrying its own guard. The forwarder's
# emission then surfaces one [ToReturn] per bundle, and
# [Sig_inst.classify_guards] evaluates each independently at the
# caller — when any path's guard holds, that path's effect fires.

def source():
    return "taint"


def sink(_x):
    pass


# ---------- Single-branch baseline ----------
# The same shape as the long-standing test_guards_to_return_python
# fixture, repeated here so the disjoint case is comparable.

def inner_single(opts):
    if len(opts["data"]) == 2:
        return source()
    return ""


def outer_single(p):
    return inner_single(p)


def single_must_fire():
    # ruleid: test-guards-disjoint
    sink(outer_single({"data": [1, 2]}))


def single_must_not_fire():
    # ok: test-guards-disjoint
    sink(outer_single({"data": [1]}))


# ---------- Disjoint branches ----------
# Both branches return a tainted value, with disjoint
# parameter-anchored guards. Every call to [inner_disjoint] returns a
# taint regardless of arg shape, so the forwarder pattern must surface
# the taint at every caller.

def inner_disjoint(opts):
    if len(opts["data"]) == 2:
        return source()
    return source()


def outer_disjoint(p):
    return inner_disjoint(p)


def disjoint_must_fire_when_inner_true():
    # len == 2 -> inner takes the True branch.
    # ruleid: test-guards-disjoint
    sink(outer_disjoint({"data": [1, 2]}))


def disjoint_must_fire_when_inner_false():
    # len != 2 -> inner takes the False branch.
    # ruleid: test-guards-disjoint
    sink(outer_disjoint({"data": [1]}))


# ---------- Disjoint branches gated by an outer condition ----------
# The forwarder conditionally dispatches to the disjoint inner; the
# outer guard composes with each inner-branch guard. When the outer
# gate is satisfied, the call must surface a finding regardless of
# which inner branch fires.

def outer_gated(p):
    if p["data"][0] != 1:
        return inner_disjoint(p)
    return ""


def gated_must_fire_inner_true():
    # data[0] = 2 != 1 -> outer if-true. len == 2 -> inner True branch.
    # ruleid: test-guards-disjoint
    sink(outer_gated({"data": [2, 3]}))


def gated_must_fire_inner_false():
    # data[0] = 3 != 1 -> outer if-true. len == 1 -> inner False branch.
    # ruleid: test-guards-disjoint
    sink(outer_gated({"data": [3]}))


def gated_must_not_fire_outer_else():
    # data[0] = 1 -> outer takes else, returns "" with no taint.
    # ok: test-guards-disjoint
    sink(outer_gated({"data": [1]}))


# ---------- Same-taint disjoint branches: complement rule fires ----------
# Both branches return the SAME taint identity (one [source()] call
# bound to a local), each under a disjoint parameter-anchored guard.
# At the forwarder's call-effect handler the two bundles share a taint
# identity, so [Taint_set.add] fuses them via [Effect_guard.compose_or],
# which dispatches to [IL_helpers.wrap_or]. The smart-constructor
# complement rule recognises [G] and [Not G] as direct syntactic
# complements and folds to [lit_bool true]: the merged bundle's guard
# becomes [top]. Outer emits one unguarded [ToReturn] and the caller
# fires unconditionally, regardless of arg shape.

def inner_shared(opts):
    x = source()
    if len(opts["data"]) == 2:
        return x
    return x


def outer_shared(p):
    return inner_shared(p)


def shared_must_fire_inner_true():
    # len == 2 -> inner True branch, but both branches return the same x.
    # ruleid: test-guards-disjoint
    sink(outer_shared({"data": [1, 2]}))


def shared_must_fire_inner_false():
    # len != 2 -> inner False branch. Same x is returned.
    # ruleid: test-guards-disjoint
    sink(outer_shared({"data": [1]}))


# ---------- Same-taint compound condition: simplification skipped, ----------
# ---------- correctness preserved by partial evaluation             ----------
# The inner condition is [a or b]. The recogniser keeps [Or] at
# TrueNode as a single compound atom and flattens it via De Morgan at
# FalseNode into [Not a, Not b]. The two ToReturn guards are therefore
# [Op Or [a; b]] and [Op And [Op Not a; Op Not b]] — De-Morgan
# equivalent to one another's negation, but not syntactic complements.
#
# At the forwarder's call-effect fold, the bundles share a taint
# identity and [Effect_guard.compose_or] runs [IL_helpers.wrap_or] on
# the two compounds. The cheap complement rule does not detect the
# De-Morgan-equivalent shape, so the merged guard stays as the
# unsimplified [Op Or [a; b; Op And [Op Not a; Op Not b]]]. Outer's
# emitted ToReturn carries this compound.
#
# This is sound: at the caller, [Sig_inst.classify_guards] substitutes
# concrete arguments and [Eval_il_partial.eval] folds the compound to
# [true] for any input — the OR is a tautology under arbitrary
# valuations. The effect fires regardless of which inner branch was
# taken.

def inner_shared_compound(opts):
    x = source()
    if len(opts["data"]) == 2 or opts["data"][0] == 1:
        return x
    return x


def outer_shared_compound(p):
    return inner_shared_compound(p)


def shared_compound_must_fire_or_satisfied_via_len():
    # len == 2 satisfies the OR -> inner True branch.
    # ruleid: test-guards-disjoint
    sink(outer_shared_compound({"data": [3, 4]}))


def shared_compound_must_fire_or_satisfied_via_first():
    # data[0] == 1 satisfies the OR -> inner True branch.
    # ruleid: test-guards-disjoint
    sink(outer_shared_compound({"data": [1, 2, 3]}))


def shared_compound_must_fire_or_violated():
    # Neither disjunct satisfied -> inner False branch.
    # ruleid: test-guards-disjoint
    sink(outer_shared_compound({"data": [3, 4, 5]}))
