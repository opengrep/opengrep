def rebind(ctx, x):
    ctx = x
    use(ctx)

def caller():
    c = "safe"
    rebind(c, source())
    # ok: test-param-rebinding-python
    sink(c)
