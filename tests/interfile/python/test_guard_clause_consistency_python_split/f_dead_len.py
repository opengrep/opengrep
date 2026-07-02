def f_dead_len(a, x):
    if len(a) == 1:
        if len(a) == 2:
            # ok: test-guard-clause-consistency
            sink(x)


