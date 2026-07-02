def test_false_positive():
    obj = IgnoresArg(source())
    result = obj.get_data()
    # ok: constructor-taint-bugs
    sink(result)
