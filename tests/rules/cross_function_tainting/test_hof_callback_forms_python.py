def source():
    return "tainted"

def sink(x):
    pass

def safe_const(_x):
    return "safe"

def passthrough(x):
    return x

def apply_cb(cb, v):
    return cb(v)

# Named lambda whose body propagates: source flows through cb to sink.
def runner_pos():
    cb = lambda x: passthrough(x)
    result = apply_cb(cb, source())
    # ruleid: test-hof-callback-forms-python
    sink(result)

# Named lambda whose body does not propagate: source must NOT reach sink.
# Regression guard for the language-agnostic dual-key over-approximation
# that registered the lambda's sig at two Function_id keys for the same
# lambda, leaving [find_by_arity] ambiguous and forcing conservative
# propagation through [apply_cb].
def runner_neg():
    cb = lambda x: safe_const(x)
    result = apply_cb(cb, source())
    # ok: test-hof-callback-forms-python
    sink(result)
