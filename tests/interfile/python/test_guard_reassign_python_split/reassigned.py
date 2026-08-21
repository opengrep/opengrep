def reassigned(x, flag):
    if flag:
        flag = recompute()
        if not flag:
            # ruleid: test-guard-reassign
            sink(x)

