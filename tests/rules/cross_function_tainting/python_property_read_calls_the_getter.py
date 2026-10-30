# Reading an attribute that a @property defines calls the getter on the
# object.
class W:
    def __init__(self, p):
        self.p = p

    @property
    def path(self):
        return self.p


def main():
    w = W(source())
    # ruleid: python_property_read_calls_the_getter
    sink(w.path)
    c = W("safe")
    # ok: python_property_read_calls_the_getter
    sink(c.path)
