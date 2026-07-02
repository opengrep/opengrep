def f_dead_branch():
    if len([1]) == 2:
        # ok: container-mutation-prune
        sink(source())
