def test_callback_only_sanitizing_lambda():
    # ok: test-hof-callback-taint
    return sink(app_callback_only(lambda x: 3, source()))

