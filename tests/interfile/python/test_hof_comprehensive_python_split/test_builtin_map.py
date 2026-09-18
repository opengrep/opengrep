from sink_and_return import sink_and_return

def test_builtin_map():
    arr = [source()]
    list(map(sink_and_return, arr))

