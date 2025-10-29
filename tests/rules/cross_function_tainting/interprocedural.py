def helper_to_sink(x):
    # ruleid: test-interprocedural-taint
    sink(x)
def helper_source (x):
    return source(x)
def helper_flow(x):
    y = helper_source(x)
    z = y
    return z
def main (input):
    x = helper_flow(input)
    helper_to_sink(x)
