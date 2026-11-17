# ERROR:
def g[*Ts](*args: *Ts) -> None:
    pass

def f[T](x: T) -> T:
    return x

def h[**P](*args: P.args) -> None:
    pass
