# Under taint_unify_mvars a sink fires on the sources whose bindings it
# unifies with, so a set keeps a source of each binding however many there
# are.
def f(c):
    x = source(a1)
    x = x + source(a2)
    x = x + source(a3)
    x = x + source(a4)
    x = x + source(a5)
    x = x + source(a6)
    x = x + source(a7)
    x = x + source(a8)
    x = x + source(a9)
    x = x + source(a10)
    # ruleid: python_unify_mvars_keeps_every_bound_source
    sink(a10, x)
    # ok: python_unify_mvars_keeps_every_bound_source
    sink(a11, x)
