def test_false_negative():
    obj = InternalSource()
    result = obj.get_data()
    # ruleid: constructor-taint-bugs
    sink(result)

