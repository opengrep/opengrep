def make(v):
    try:
        risky()
    except Exception as e:
        e.data = v
        return lambda: e.data


def same_call():
    b = make(source())
    # ruleid: test-catch-variable-captured-python
    sink(b())


def other_call():
    a = make(source())
    b = make("clean")
    # ok: test-catch-variable-captured-python
    sink(b())
