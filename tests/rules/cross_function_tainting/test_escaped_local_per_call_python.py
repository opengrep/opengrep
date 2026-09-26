def make(v):
    x = v
    return lambda: x


def make_pair():
    x = []

    def add(v):
        x.append(v)

    def get():
        return x

    return add, get


def other_call_is_clean():
    a = make(source())
    b = make("clean")
    # ok: test-escaped-local-per-call-python
    sink(b())


def same_call_is_tainted():
    b = make(source())
    # ruleid: test-escaped-local-per-call-python
    sink(b())


def closures_of_one_call_share():
    add, get = make_pair()
    add(source())
    # ruleid: test-escaped-local-per-call-python
    sink(get())


def closures_of_two_calls_do_not_share():
    add_a, get_a = make_pair()
    add_b, get_b = make_pair()
    add_a(source())
    # ok: test-escaped-local-per-call-python
    sink(get_b())
