def foo():
    x = source()
    y = 1
    match y:
        # ruleid: test
        case 1 if sink(x):
            pass
        # OK:
        case 2 if sink("clean"):
            pass

def bar():
    match 1:
        # ruleid: test
        case _ if sink(source()):
            pass
