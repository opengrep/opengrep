class Box:
    pass

def f(o, v):
    o.f = v
    o = Box()
    use(o)

def g(box, v):
    box.append(v)
    box = []
    use(box)

def caller():
    b = Box()
    f(b, source())
    # ruleid: test-mutate-then-rebind-python
    sink(b.f)

def caller2():
    l = []
    g(l, source())
    # ruleid: test-mutate-then-rebind-python
    sink(l)
