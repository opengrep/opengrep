def pass_taint(x):
    y = x
    return y

def go():
    a = source()
    b = pass_taint(a)
    sink(b)
