def source():
    return ""


def cond():
    return False


# Mutually-recursive cycle p -> q -> r -> p.  The taint SOURCE is injected
# INSIDE the cycle (q returns r(source())) and must flow around the cycle to
# become p's return value.  Computing p's return signature needs q's, which
# needs r's, which needs p's -- a cyclic dependency.  A single topological pass
# summarises one member before its mutual callee and loses the return-taint;
# the SCC signature fixpoint converges them.
def p(x):
    return q(x)


def q(x):
    return r(source())


def r(x):
    if cond():
        return p(x)
    return x
