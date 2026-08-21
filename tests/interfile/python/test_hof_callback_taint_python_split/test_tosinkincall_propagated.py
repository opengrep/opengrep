def test_tosinkincall_propagated():
    # ruleid: test-hof-callback-taint
    return sink(wrapper_propagates_callback(propagates, source()))

