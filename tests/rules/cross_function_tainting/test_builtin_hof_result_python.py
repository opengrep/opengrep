def mapped(ids):
    ys = map(lambda i: source(), ids)
    # ruleid: test-builtin-hof-result-python
    sink(ys)


def filtered(ids):
    ys = filter(lambda i: source(), ids)
    # ok: test-builtin-hof-result-python
    sink(ys)


def sorted_by_key(ids):
    ys = sorted(ids, key=lambda i: source())
    # ok: test-builtin-hof-result-python
    sink(ys)


def filtered_tainted():
    ys = filter(lambda i: i, [source()])
    # ruleid: test-builtin-hof-result-python
    sink(ys)


def sorted_tainted():
    ys = sorted([source()], key=lambda i: i)
    # ruleid: test-builtin-hof-result-python
    sink(ys)
