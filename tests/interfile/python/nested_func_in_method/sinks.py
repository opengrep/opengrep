def leak_nested(v):
    # ruleid: nested-func-in-method
    sink(v)


def leak_top(v):
    # ruleid: nested-func-in-method
    sink(v)


def leak_method(v):
    # ruleid: nested-func-in-method
    sink(v)
