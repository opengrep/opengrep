from sink import sink

def f_dead_str(a, x):
    if a == "get":
        if a == "post":
            # ok: test-guard-clause-consistency
            sink(x)


