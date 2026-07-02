# This file lexically precedes zzz.py.  Its Foo has NO parent.
# Under first-wins semantics for set_parent, this Foo's binding
# would silently win in Type_state, breaking the super().__init__
# chain at the call site in main.py.
class Foo:
    def __init__(self, value):
        self.value = value
