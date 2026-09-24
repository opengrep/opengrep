from closures import apply, pick, pick_rev, source


def returned_closures(c):
    # ruleid: closure-join-across-files
    sink(pick(c)())
    # ruleid: closure-join-across-files
    sink(pick_rev(c)())


def callback_chosen_by_branch(c):
    x = source()
    if c:
        g = lambda a: print(a)
    else:
        # ruleid: closure-join-across-files
        g = lambda a: sink(a)
    apply(g, x)
