def source():
    return ""


def pick(c):
    x = source()
    return (lambda: x) if c else (lambda: 0)


def pick_rev(c):
    x = source()
    return (lambda: 0) if c else (lambda: x)


def apply(cb, v):
    cb(v)
