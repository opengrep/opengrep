def test_tosinkincall_propagated_sanitized():
    # ok: test-hof-callback-taint
    return sink(wrapper_propagates_callback(sanitizes, source()))

