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

def source(): return "taint"
def sink(_x): pass

def grow(x):
    x.append(2)


# Helper-call mutation: `grow(arr)` mutates `arr` via its parameter.
# Without the fix, `arr`'s `Sym (List [1])` survives the call and the
# pruner folds `len(arr) == 2` to False.
def f_helper_call():
    arr = [1]
    grow(arr)
    if len(arr) == 2:
        # ruleid: container-mutation-prune
        sink(source())


# Method-call mutation: `arr.append(...)` already invalidates via the
# existing void-method case in the transfer.
def f_method_call():
    arr = [1]
    arr.append(2)
    if len(arr) == 2:
        # ruleid: container-mutation-prune
        sink(source())


# Indexed-write mutation: hits the catch-all that invalidates the
# instruction's lvar.
def f_indexed_write():
    arr = [1]
    arr[0] = 99
    if len(arr) == 1:
        # ruleid: container-mutation-prune
        sink(source())


# Sibling container untouched: `grow(a)` does not touch `b`, so
# `b`'s `Sym ([1, 2])` survives and `len(b) == 2` correctly folds to
# True. The fix is per-name, not global, so this still works.
def f_sibling_untouched():
    a = [1]
    b = [1, 2]
    grow(a)
    if len(b) == 2:
        # ruleid: container-mutation-prune
        sink(source())


# Negative: fresh literal, no mutation, dead branch. The pruner
# correctly folds `len([1]) == 2` to False and drops the sink.
# Asserts that we did not over-invalidate and break legitimate
# pruning.
def f_dead_branch():
    if len([1]) == 2:
        # ok: container-mutation-prune
        sink(source())
