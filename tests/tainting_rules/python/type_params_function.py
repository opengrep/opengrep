def f[T](x: T) -> T:
    y = source()
    # ruleid: taint
    sink(y)
    return x

def g[T](x: T) -> T:
    y = source()
    # ok: taint
    sink(x)
    return y
