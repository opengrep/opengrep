# The constructor calls a method that reads a field the constructor stored.

class Tainted:
    def __init__(self):
        self.path = source()
        self.load()

    def load(self):
        # ruleid: test-constructor-calls-method-reading-field
        sink(self.path)


class Clean:
    def __init__(self):
        self.path = "constant"
        self.load()

    def load(self):
        # ok: test-constructor-calls-method-reading-field
        sink(self.path)
