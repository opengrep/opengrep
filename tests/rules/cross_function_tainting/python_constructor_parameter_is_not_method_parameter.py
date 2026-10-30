# A field that the constructor sets from its parameter does not hold the
# argument of a method at the same position.
class T:
    def __init__(self, a):
        self.f = a
        self.g = source()

    def m(self, x):
        # ok: python_constructor_parameter_is_not_method_parameter
        sink(self.f)
        # ruleid: python_constructor_parameter_is_not_method_parameter
        sink(self.g)

def use():
    t = T("c")
    t.m(source())
