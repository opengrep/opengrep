class Box:
    pass


def rebind(ctx, x):
    ctx = x
    use(ctx)


def fill_then_rebind(o, v):
    o.f = v
    o = Box()
    use(o)
