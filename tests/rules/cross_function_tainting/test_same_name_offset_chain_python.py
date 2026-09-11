def read_deep(o):
    # The signature lval is 'o.data.data: two distinct fields sharing a
    # name.  The composition cycle guard must not truncate the chain to
    # 'o.data — only a repeated occurrence of the SAME field token (a
    # real x = x.getX() cycle) is a cycle.
    return o.data.data


def read_other(o):
    return o.other


def main():
    o = build()
    o.data.data = taint()
    # ruleid: same-name-offset-chain
    sink(read_deep(o))
    # ok: same-name-offset-chain
    sink(read_other(o))
