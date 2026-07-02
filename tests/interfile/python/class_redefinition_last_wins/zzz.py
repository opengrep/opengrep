from base import Base


# This file lexically follows aaa.py.  Its Foo extends Base.
# Last-wins semantics for set_parent means this is the binding
# that lives in Type_state.parent_class[Foo], so super().__init__
# resolves to Base.__init__ where the sink lives.
class Foo(Base):
    def __init__(self, value):
        super().__init__(value)
