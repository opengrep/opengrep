# Destructuring assignment whose targets are not plain names. These reach
# AST_to_IL through the expr_to_pattern fallback and bind to the real lval,
# so the destructured value actually reaches them.


def attribute_targets(o):
    o.a, o.b = taint_source(), 1

    # ruleid: taint-python-destructuring-assign
    sink(o.a)

    # Each slot keeps its own index, so the clean slot stays clean.
    # ok: taint-python-destructuring-assign
    sink(o.b)


def index_targets(d):
    d[0], d[1] = taint_source(), 1

    # ruleid: taint-python-destructuring-assign
    sink(d[0])

    # ok: taint-python-destructuring-assign
    sink(d[1])


def plain_name_targets():
    a, b = taint_source(), 1

    # ruleid: taint-python-destructuring-assign
    sink(a)

    # ok: taint-python-destructuring-assign
    sink(b)
