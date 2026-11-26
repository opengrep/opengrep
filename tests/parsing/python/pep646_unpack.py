# PEP 646: starred expressions in subscripts and in the annotation of *args
# (Python 3.11+)

def f(*args: *Ts) -> None:
    pass

def g(*args: *tuple[int, ...]) -> None:
    pass

def h() -> R[*tuple[A, ...]]:
    pass

def i() -> R[Unpack[tuple[A, ...]]]:
    pass

x: tuple[*Ts] = ()
y = a[*idx]
z = a[1, *idx]
w = a[*b, *c]
v = a[*b or c]
