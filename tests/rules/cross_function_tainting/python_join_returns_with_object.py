def two_returns(x, c):
    if c:
        return x
    return (x, "")


def one_return(x, c):
    return x if c else (x, "")


def caller(self, c):
    p, q = two_returns(self.path, c)
    # ruleid: python_join_returns_with_object
    sink(p)
    # ruleid: python_join_returns_with_object
    sink(q)
    p2, q2 = one_return(self.path, c)
    # ruleid: python_join_returns_with_object
    sink(p2)
    # ruleid: python_join_returns_with_object
    sink(q2)
