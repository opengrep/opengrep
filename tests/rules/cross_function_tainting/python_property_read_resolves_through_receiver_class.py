# A read of an attribute calls a getter of the class of the object only.
import os

class A:
    @property
    def name(self):
        return source()

    def m(self):
        # ruleid: python_property_read_resolves_through_receiver_class
        sink(self.name)

class B:
    def __init__(self):
        self.name = "safe"

    def m(self):
        # ok: python_property_read_resolves_through_receiver_class
        sink(self.name)

def typed(a: A):
    # ruleid: python_property_read_resolves_through_receiver_class
    sink(a.name)

def untyped(d):
    # ok: python_property_read_resolves_through_receiver_class
    sink(d.name)

def module():
    # ok: python_property_read_resolves_through_receiver_class
    sink(os.name)
