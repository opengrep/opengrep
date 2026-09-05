# A method value of a receiver with no declared type, passed as a callback:
# resolved to the class's method by its leaf name.


def source():
    return "taint"


def sink(_x):
    pass


class Handler:
    def run(self, v):
        # ruleid: test-hof-method-value-python
        sink(v)


def apply(cb, x):
    cb(x)


def main(handler):
    apply(handler.run, source())
