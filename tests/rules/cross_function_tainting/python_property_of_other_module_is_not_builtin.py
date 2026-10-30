# A decorator that refers to another module's [property] is not the
# builtin.
import mylib

class A:
    @mylib.property
    def name(self):
        return source()

class B:
    @property
    def name(self):
        return source()

def f(a: A, b: B):
    # ok: python_property_of_other_module_is_not_builtin
    sink(a.name)
    # ruleid: python_property_of_other_module_is_not_builtin
    sink(b.name)
