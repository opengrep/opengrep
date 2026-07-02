def f(a, b, x):
    g = ""
    g2 = ""
    i = 0
    while i < 3:
        # ruleid: test-guard-env-widen
        sink(g)
        if a == 2:
            g = x
        if b == 5:
            g = g2
        if a == 3:
            g2 = x
        i = i + 1


