# Pins the taint_MAX_POLY_OFFSET (=4) truncation behaviour: composed
# poly-taint offsets drop segments past the cap, so taint written 5 fields
# deep is recorded on the depth-4 prefix. The deep read still matches (the
# prefix is tainted) and — the documented over-approximation — so does any
# sibling under the kept prefix. A field outside the prefix stays clean.


def f5(x):
    x.e = taint()


def f4(x):
    f5(x.d)


def f3(x):
    f4(x.c)


def f2(x):
    f3(x.b)


def f1(x):
    # composes to x.a.b.c.d.e — five segments, truncated to x.a.b.c.d
    f2(x.a)


def deep_flow(obj):
    f1(obj)
    # ruleid: poly-offset-cap
    sink(obj.a.b.c.d.e)


def sibling_past_cap(obj):
    f1(obj)
    # The cap collapses .e and .f into the same depth-4 prefix, so this
    # sibling reads as tainted — the pinned false-positive trade-off.
    # ruleid: poly-offset-cap
    sink(obj.a.b.c.d.f)


def outside_prefix(obj):
    f1(obj)
    # ok: poly-offset-cap
    sink(obj.a.x)
