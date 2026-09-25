# A method call on self looks the method up on the instance's class at run
# time, so a call in a base class reaches the overrides of its subclasses.
class Base:
    def run(self, x):
        self.m(x)

    def m(self, x):
        return x


class Unrelated:
    def m(self, x):
        # ok: self_call_reaches_override_in_other_file
        sink(x)
