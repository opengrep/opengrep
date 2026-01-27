# Test: Inline lambda call taint propagation
# With taint_intrafile: 1 finding
# The taint should flow from source() through the inline lambda call to sink()

def f(x):
    z = source(x)
    # ruleid: lambda-inline-call-taint
    (lambda k: sink(k))(z)
