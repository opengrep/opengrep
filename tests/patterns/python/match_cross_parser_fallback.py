def f(x):
    # ERROR:
    match x:
        case 1:
            print("a")

def g(x):
    # ERROR:
    match x:
        case 1 if x > 0:
            print("b")
