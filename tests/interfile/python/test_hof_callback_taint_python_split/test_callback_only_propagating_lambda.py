def test_callback_only_propagating_lambda():
    # ruleid: test-hof-callback-taint
    return sink(app_callback_only(lambda x: x, source()))

