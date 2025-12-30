# Test: Top-level HOF (Higher-Order Function) callback taint propagation
# With taint_intrafile: 2 findings
# 1. Lambda at top level: g = lambda x: sink(x); g(source())
# 2. User-defined HOF: apply_fn(process, source()) where process calls sink

# Lambda - gets inlined
# ruleid: toplevel-hof-callback-taint
g = lambda x: sink(x)
g(source())

# Wrapper function - has signature with ToSink
def process(x):
   # ruleid: toplevel-hof-callback-taint
   sink(x)

def apply_fn(fn, x):
    fn(x)

apply_fn(process, source())
