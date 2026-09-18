# Test: Bare sink name passed as a HOF callback (gap B from issue #499).
# `sink` has no definition, so no signature exists to instantiate; the
# argument-to-parameter symbolic pass stamps `fn` with Sym `sink` (every
# call site agrees on the binding) and the sink pattern matches fn(x).

def apply_fn(fn, x):
    # ruleid: test-hof-bare-sink-callback
    fn(x)

apply_fn(sink, source())
