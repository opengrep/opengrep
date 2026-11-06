def apply(f, v):
    f(v)


def _tmp_handler(v):
    # ruleid: python_function_named_tmp_through_variable
    sink(v)


def _tmp_constant_handler(v):
    # ok: python_function_named_tmp_through_variable
    sink(v)


def go():
    f = _tmp_handler
    apply(f, source())


def go_constant():
    f = _tmp_constant_handler
    apply(f, "constant")
