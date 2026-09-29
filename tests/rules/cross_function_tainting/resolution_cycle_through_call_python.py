# The value of g is f, a member access on a call of g itself. A function
# assigns g too, so a call of g inside a function reaches every value of g,
# h among them.
def h():
    return source()


def clean():
    return 0


g = h
f = g().m
g = f


def reset():
    global g
    g = h


def run():
    # ruleid: resolution_cycle_through_call_python
    sink(g())
    # ok: resolution_cycle_through_call_python
    sink(clean())
