from sink_and_return_true import sink_and_return_true

def test_builtin_filter():
    arr = [source()]
    list(filter(sink_and_return_true, arr))

