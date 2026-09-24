def apply(f):
    return f()


def call_only(f):
    f()
    return "clean"


def sink_it(f):
    # ruleid: test-callback-result-py
    sink(f())


def store(box, f):
    box.v = f()


def wrap(handler):
    def run(c):
        res = handler(c)
        use(res)

    return run


def tainted():
    return source()


def test(box):
    # ruleid: test-callback-result-py
    sink(apply(tainted))
    # ok: test-callback-result-py
    sink(call_only(tainted))
    sink_it(tainted)
    store(box, tainted)
    # ruleid: test-callback-result-py
    sink(box.v)
    # ok: test-callback-result-py
    sink(wrap(tainted))
