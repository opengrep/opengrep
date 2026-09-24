def lambda_in_variable():
    u = source()
    f = lambda: u
    # ruleid: test-closure-captured-py
    sink(f())


def immediately_called():
    u = source()
    # ruleid: test-closure-captured-py
    sink((lambda: u)())


def nested_def():
    u = source()

    def g():
        return u

    # ruleid: test-closure-captured-py
    sink(g())


def tainted_after_creation():
    u = "clean"
    f = lambda: u
    u = source()
    # ruleid: test-closure-captured-py
    sink(f())


def with_(cb):
    return cb()


def callback(u):
    # ruleid: test-closure-captured-py
    sink(with_(lambda: u))


def captured_param_beside_own(u):
    f = lambda x: u
    # ruleid: test-closure-captured-py
    sink(f("clean"))


def own_param_beside_captured(u):
    f = lambda x: u
    # ok: test-closure-captured-py
    sink(f(source()))


def mk_local():
    u = source()
    return lambda: u


def returned_over_local():
    f = mk_local()
    # ruleid: test-closure-captured-py
    sink(f())


def mk_param(p):
    return lambda: p


def returned_over_param():
    g = mk_param(source())
    # ruleid: test-closure-captured-py
    sink(g())


def returned_over_clean_param():
    g = mk_param("clean")
    # ok: test-closure-captured-py
    sink(g())


def mk_pair():
    box = []

    def add(v):
        box.append(v)

    def get():
        return box

    return add, get


def shared_local():
    add, get = mk_pair()
    add(source())
    # ruleid: test-closure-captured-py
    sink(get())


def main():
    callback(source())
    captured_param_beside_own(source())
    own_param_beside_captured("clean")
