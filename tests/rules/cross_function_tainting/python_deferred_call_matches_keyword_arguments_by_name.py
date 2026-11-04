# A call through a callback with keyword arguments gives each argument to the
# parameter of its name.
def first(x, y):
    return x


def mk():
    return lambda cb, a, b: cb(y=a, x=b)


def main():
    f = mk()
    # ruleid: python_deferred_call_matches_keyword_arguments_by_name
    sink(f(first, "safe", source()))
    # ok: python_deferred_call_matches_keyword_arguments_by_name
    sink(f(first, source(), "safe"))
