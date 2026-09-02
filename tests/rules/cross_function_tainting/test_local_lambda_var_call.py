# Test: Lambda assigned to local var, called in same function with tainted arg
# Reported as broken in issue #499 by maintainer (dimitris-m), but actually works.
# The lambda's body is inlined/substituted when called through the bound var,
# allowing taint to flow via the call argument (not just closure capture).

def f(x):
    z = source(x)
    # ruleid: test-local-lambda-var-call
    g = lambda k: sink(k)
    g(z)
