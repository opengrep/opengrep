def foo():
    # capture pattern: `case x:` binds x to the whole subject.
    x = source()
    match x:
        case captured:
            # ruleid: test
            sink(captured)

def bar():
    # sequence destructuring: each element inherits the subject's taint.
    xs = [source(), 1]
    match xs:
        case [a, b]:
            # ruleid: test
            sink(a)

def baz():
    xs = [1, 2]
    match xs:
        case [a, b]:
            # OK:
            sink(a)
