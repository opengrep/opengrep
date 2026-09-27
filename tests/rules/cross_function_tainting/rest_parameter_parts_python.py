def attribute(*xs):
    return xs.count


def first(*xs):
    return xs[0]


def second(*xs):
    return xs[1]


def whole(*xs):
    return xs


def sink_whole(*xs):
    # ruleid: rest_parameter_parts_python
    sink(xs)


def test():
    # ok: rest_parameter_parts_python
    sink(attribute(source(), "x"))
    # ruleid: rest_parameter_parts_python
    sink(first(source(), "x"))
    # ok: rest_parameter_parts_python
    sink(second(source(), "x"))
    # ruleid: rest_parameter_parts_python
    sink(whole(source(), "x"))
    sink_whole(source(), "x")
