from sanitizes import sanitizes
from wrapper_propagates_callback import wrapper_propagates_callback

def test_tosinkincall_propagated_sanitized():
    # ok: test-hof-callback-taint
    return sink(wrapper_propagates_callback(sanitizes, source()))

