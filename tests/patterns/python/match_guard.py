def f(x):
    # ERROR:
    match x:
        case 1 if x > 0:
            print("guarded one")

def g(x):
    match x:
        case 1:
            print("unguarded: not expected to match a guarded pattern")

def h(x):
    # ERROR:
    match x:
        case 2 if x < 0:
            print("guarded two")
