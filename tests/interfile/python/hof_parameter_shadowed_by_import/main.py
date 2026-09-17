from a import callback


def propagates(x):
    return x


def real_hof(callback, value):
    return callback(value)


def test_real_hof():
    # ruleid: hof-parameter-shadowed-by-import
    sink(real_hof(propagates, source()))
