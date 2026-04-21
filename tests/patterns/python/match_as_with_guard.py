def f(x):
    # ERROR:
    match x:
        case 1 as captured if x > 0:
            print("as with guard")

def g(x):
    match x:
        case 1 as captured:
            print("as without guard")

def h(x):
    match x:
        case 1 if x > 0:
            print("guard without as")

def i(x):
    match x:
        case 1:
            print("neither")
