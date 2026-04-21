def f(x):
    # ERROR:
    match x:
        case 1:
            print("a")

def g(x):
    # no match: the guard is now preserved in the AST so an unguarded
    # pattern does not match a guarded case (PEP 634).
    match x:
        case 1 if x > 0:
            print("b")
