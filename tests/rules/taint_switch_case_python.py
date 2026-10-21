def cases_do_not_fall_through(k):
    y = 0
    match k:
        case 1:
            y = taint_source()
        case _:
            # ok: taint-switch-case-python
            sink(y)
