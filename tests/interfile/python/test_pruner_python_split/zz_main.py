# Unreachable-branch pruning: a statically dead branch must not produce
# findings, must not detect fresh sources, must not contribute to the
# function's signature, and must not leak its writes past the Join with
# a live branch.

# Pre-existing tainted lval, sink in dead branch.
# Fresh source produced inside the dead branch.
# Fresh source via an intermediate local.
# Symmetric direction — dead else of `if True`.
# Constant-folded condition (length over a known-length literal).
# Nested dead branches.
# Post-Join leakage: dead branch's write must not survive the Join.
# Function whose ToReturn comes only from a dead branch — caller
# must not fire on the result.
# Live counterpart — sanity check that the analysis still fires for
# code that isn't dead.
