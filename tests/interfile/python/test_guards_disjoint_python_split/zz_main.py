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

# ---------- Single-branch baseline ----------
# The same shape as the long-standing test_guards_to_return_python
# fixture, repeated here so the disjoint case is comparable.

# ---------- Disjoint branches ----------
# Both branches return a tainted value, with disjoint
# parameter-anchored guards. Every call to [inner_disjoint] returns a
# taint regardless of arg shape, so the forwarder pattern must surface
# the taint at every caller.

# ---------- Disjoint branches gated by an outer condition ----------
# The forwarder conditionally dispatches to the disjoint inner; the
# outer guard composes with each inner-branch guard. When the outer
# gate is satisfied, the call must surface a finding regardless of
# which inner branch fires.

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

