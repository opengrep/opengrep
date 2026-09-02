# Test: Mutual recursion in intrafile mode (taint_intrafile: true)
# With taint_intrafile enabled, SCC-aware signature fixpoint should handle cycles
# in the call graph and detect taint flowing through mutually-recursive functions.

def func_a(x):
    if False:
        return x
    return func_b(x)

def func_b(x):
    # ruleid: test-mutual-recursion-intrafile
    sink(x)
    return func_a(x)

func_a(source())
