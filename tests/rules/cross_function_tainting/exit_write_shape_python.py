class Box:
    def __init__(self):
        self.x = None


class P:
    def __init__(self, a, b):
        self.a = a
        self.b = b


def put(o, p):
    o.x = p


def record_through_parameter():
    o = Box()
    p = P(source(), "safe")
    put(o, p)
    # ruleid: exit_write_shape_python
    sink(o.x.a)
    # ok: exit_write_shape_python
    sink(o.x.b)


def put_function(o, f):
    o.cb = f


def function_through_parameter():
    o = Box()
    put_function(o, lambda: source())
    # ruleid: exit_write_shape_python
    sink(o.cb())


def put_lambda(o):
    o.cb = lambda: source()


def lambda_written_in_callee():
    o = Box()
    put_lambda(o)
    # ruleid: exit_write_shape_python
    sink(o.cb())


def clean_function():
    o = Box()
    put_function(o, lambda: "safe")
    # ok: exit_write_shape_python
    sink(o.cb())
