def constant_reaches_handler_after_four_calls():
    x = "secret"
    try:
        a()
        b()
        c()
        d()
    except Exception:
        # ruleid: cp_handler_after_four_calls
        sink(x)


def other_value_reaches_handler():
    x = "public"
    try:
        a()
        b()
        c()
        d()
    except Exception:
        # ok: cp_handler_after_four_calls
        sink(x)
