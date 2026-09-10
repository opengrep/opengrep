# A method value of a receiver with no declared type is passed as a callback,
# and it resolves to the class's method by its bare name.


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
