def foo():
    x = source()
    match x:
        case _ as captured:
            # ruleid: test
            sink(captured)

def bar():
    x = "clean"
    match x:
        case _ as captured:
            # OK:
            sink(captured)

def baz():
    # nested as: only `inner` carries the taint from the list element.
    xs = [source(), 1]
    match xs:
        case [a as inner, b]:
            # ruleid: test
            sink(inner)
