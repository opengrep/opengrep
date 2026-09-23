# Test that parameter shadowing does not produce false HOF detection.

def propagates(x):
    return x

# True HOF -- callback IS the parameter, should propagate taint.
def real_hof(callback, value):
    return callback(value)

def test_real_hof():
    # ruleid: test-hof-shadowing
    sink(real_hof(propagates, source()))

# False HOF -- callback is SHADOWED by a local, should NOT be detected as HOF.
def not_a_hof(callback, value):
    callback = lambda x: 3  # shadows the parameter
    return callback(value)   # calls the local, not the original parameter

def test_not_a_hof():
    # ok: test-hof-shadowing
    sink(not_a_hof(propagates, source()))
