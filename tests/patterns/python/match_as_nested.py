def f(x):
    # ERROR:
    match x:
        case [a, b as second]:
            print("nested as in list")

def g(x):
    match x:
        case [a, b]:
            print("no nested as")

def h(x):
    match x:
        case a as captured:
            print("top-level as, not nested")
