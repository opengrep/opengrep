def test_direct_flow_propagating_named():
    # ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(propagates, source()))

