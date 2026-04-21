def f(x):
    # ERROR:
    match x:
        case 1 as captured if x > 0:
            print("as + guard, should match `case $P if $G:`")

def g(x):
    # ERROR:
    match x:
        case 1 if x > 0:
            print("guard only, should match")

def h(x):
    match x:
        case 1 as captured:
            print("as only, no guard, should NOT match")

def i(x):
    match x:
        case 1:
            print("neither, should NOT match")
