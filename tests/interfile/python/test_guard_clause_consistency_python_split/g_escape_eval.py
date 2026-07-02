def g_escape_eval(s, x):
    if s == "a\nb":
        # ok: test-guard-clause-consistency
        sink(x)


