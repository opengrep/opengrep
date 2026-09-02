# Test: Inheritance / method override in intrafile mode (taint_intrafile: true)
# Virtual dispatch should resolve method calls on base-class types to their
# overridden implementations in derived classes, propagating taint signatures.

class Base:
    def process(self, x):
        return x

class Derived(Base):
    def process(self, x):
        # ruleid: test-inheritance-intrafile
        sink(x)

obj = Derived()
obj.process(source())
