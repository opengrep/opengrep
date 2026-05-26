# Effect guards must not survive a reassignment of the variable they read.
# The IL is non-SSA: the same name denotes the new value after an
# assignment, so a guard like `flag && !flag` accumulated across a
# reassignment is not a contradiction and must not drop the effect.

def source():
    return "taint"

def sink(_x):
    pass

def recompute():
    return False


# The guard variable is reassigned between the two opposite-polarity
# checks, so the inner branch is reachable and the taint must reach the
# sink. (On `main` this fired; without the reassignment-kill it is lost.)
def reassigned(x, flag):
    if flag:
        flag = recompute()
        if not flag:
            # ruleid: test-guard-reassign
            sink(x)

def call_reassigned():
    reassigned(source(), True)


# No reassignment between the checks: `flag` cannot be both true and
# false, so the inner branch is genuinely unreachable and the guard
# correctly prunes the effect.
def dead(x, flag):
    if flag:
        if not flag:
            # ok: test-guard-reassign
            sink(x)

def call_dead():
    dead(source(), True)
