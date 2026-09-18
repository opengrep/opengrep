from propagates import propagates
from wrapper_propagates_callback import wrapper_propagates_callback

def test_tosinkincall_propagated():
    # ruleid: test-hof-callback-taint
    return sink(wrapper_propagates_callback(propagates, source()))

