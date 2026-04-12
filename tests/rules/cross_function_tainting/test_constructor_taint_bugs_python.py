# FALSE NEGATIVE: zero-arg constructor with internal source.
class InternalSource:
    def __init__(self):
        self.data = source()

    def get_data(self):
        return self.data

def test_false_negative():
    obj = InternalSource()
    result = obj.get_data()
    # ruleid: constructor-taint-bugs
    sink(result)

# FALSE POSITIVE: constructor ignores its argument.
class IgnoresArg:
    def __init__(self, data):
        self.data = "safe"

    def get_data(self):
        return self.data

def test_false_positive():
    obj = IgnoresArg(source())
    result = obj.get_data()
    # ok: constructor-taint-bugs
    sink(result)
