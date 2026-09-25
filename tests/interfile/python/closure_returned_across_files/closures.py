def source():
    return ""


def mk_local():
    u = source()
    return lambda: u


def mk_pair():
    box = []

    def add(v):
        box.append(v)

    def get():
        return box

    return add, get
