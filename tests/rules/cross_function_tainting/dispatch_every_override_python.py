# Python selects a method by its name alone, so a call through the base class
# reaches the override of that name in every subclass.
class Visitor:
    def visit(self, node, s):
        return s

    def run(self, node, s):
        self.visit(node, s)


class First(Visitor):
    def visit(self, node, s):
        # ruleid: dispatch_every_override_python
        sink(s)


class Second(Visitor):
    def visit(self, node, s):
        # ruleid: dispatch_every_override_python
        sink(s)


class Unrelated:
    def visit(self, node, s):
        # ok: dispatch_every_override_python
        sink(s)


def main(v):
    Visitor().run(1, source())
