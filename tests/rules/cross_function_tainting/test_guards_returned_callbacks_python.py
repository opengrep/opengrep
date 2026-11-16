def make(a, x):
    if a == 1:
        def first(b):
            if b == 1:
                return x
            return ""
        return first
    def second(b):
        if b == 2:
            return x
        return ""
    return second


def make_swapped(a, x):
    if a != 1:
        def second(b):
            if b == 2:
                return x
            return ""
        return second
    def first(b):
        if b == 1:
            return x
        return ""
    return first


def via_first():
    f = make(1, source())
    # ruleid: test-guards-returned-callbacks
    sink(f(1))
    # ok: test-guards-returned-callbacks
    sink(f(2))


def via_second():
    f = make(0, source())
    # ruleid: test-guards-returned-callbacks
    sink(f(2))
    # ok: test-guards-returned-callbacks
    sink(f(1))


def via_first_swapped():
    f = make_swapped(1, source())
    # ruleid: test-guards-returned-callbacks
    sink(f(1))
    # ok: test-guards-returned-callbacks
    sink(f(2))


def via_second_swapped():
    f = make_swapped(0, source())
    # ruleid: test-guards-returned-callbacks
    sink(f(2))
    # ok: test-guards-returned-callbacks
    sink(f(1))
