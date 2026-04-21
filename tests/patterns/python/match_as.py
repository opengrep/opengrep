def f(x):
    # ERROR:
    match x:
        case 1 as captured:
            print("top-level as")

def g(x):
    match x:
        case 1:
            print("no as")

def h(x):
    match x:
        case [a, b as second]:
            print("nested as only, pattern expects top-level as")
