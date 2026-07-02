def dead(x, flag):
    if flag:
        if not flag:
            # ok: test-guard-reassign
            sink(x)

