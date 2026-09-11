def test_builtin_map():
    arr = [source()]
    list(map(sink_and_return, arr))

