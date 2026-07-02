# `apply` is the HOF whose signature carries the ToSinkInCall
# obligation { callee = f (param 0), arg = 1 (= x) }.  Its signature
# is extracted here and replayed at the call site in main.py.
def apply(f, x):
    return f(x)
