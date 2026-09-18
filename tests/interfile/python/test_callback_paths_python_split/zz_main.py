# A callback variable assigned at different offsets of the same
# parameter across the branches of an if/else: each alternative offset
# is preserved at the Join and the engine dispatches to every resolved
# callback at the call site. Earlier the unify of two Arg shapes for
# the same parameter took the longest common prefix of their offset
# paths; for divergent paths this collapsed to the empty offset and
# the dispatcher could not resolve any callback. Now the Arg shape
# carries every alternative offset.

# Two-way branch — the reviewer's case. Both handlers must fire.
# Three-way branch — three distinct paths must all fire.
# Nested branches — paths of different depths. The outer chooses
# between two sub-trees; the inner picks within each. Four callbacks
# at offsets that LCP would have collapsed entirely.
# Same callback bound in both branches — fires once (sort_uniq dedup).
# Negative: callback that is NOT bound by any branch must not fire,
# even though it lives at a sibling key in opts.
