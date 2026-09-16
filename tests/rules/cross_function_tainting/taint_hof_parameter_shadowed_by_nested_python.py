def not_a_hof(x):
    def callback(y):
        return 3
    return callback(x)

def propagates(x):
    return x

def real_hof(callback, value):
    return callback(value)

def test_real_hof():
    # ruleid: taint-hof-parameter-shadowed-by-nested-python
    sink(real_hof(propagates, source()))
