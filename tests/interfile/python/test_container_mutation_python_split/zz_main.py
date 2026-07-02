# Regression for a false negative produced by the combination of
# `Container` in `Dataflow_svalue.is_symbolic_expr` (so a literal list
# becomes a `Sym (Container ...)` snapshot) and
# `prune_branch_if_unreachable` (which folds `len(arr) == N` against
# that snapshot to mark the wrong branch dead). When a container is
# mutated through an alias the snapshot goes stale; without
# invalidation, the pruner trusts it and silently drops a sink that
# is reachable at runtime.
#
# The fix invalidates the symbolic value of any bare-var argument
# whose current state is a container `Sym` before a call is
# processed. Method-call and indexed-write mutations were already
# handled; the new behaviour covers helper-call mutation.

# Helper-call mutation: `grow(arr)` mutates `arr` via its parameter.
# Without the fix, `arr`'s `Sym (List [1])` survives the call and the
# pruner folds `len(arr) == 2` to False.
# Method-call mutation: `arr.append(...)` already invalidates via the
# existing void-method case in the transfer.
# Indexed-write mutation: hits the catch-all that invalidates the
# instruction's lvar.
# Sibling container untouched: `grow(a)` does not touch `b`, so
# `b`'s `Sym ([1, 2])` survives and `len(b) == 2` correctly folds to
# True. The fix is per-name, not global, so this still works.
# Negative: fresh literal, no mutation, dead branch. The pruner
# correctly folds `len([1]) == 2` to False and drops the sink.
# Asserts that we did not over-invalidate and break legitimate
# pruning.
