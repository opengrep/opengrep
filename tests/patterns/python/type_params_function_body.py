def first[T](xs: list[T]) -> T:
    # ERROR:
    dangerous_call()
    return xs[0]

def control(xs):
    safe_call()
