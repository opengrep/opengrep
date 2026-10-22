def cases_do_not_fall_through(k):
    y = 0
    match k:
        case 1:
            y = taint_source()
        case _:
            # ok: taint-switch-case-python3
            sink(y)


def later_case_is_reachable(k):
    x = taint_source()
    match k:
        case 1:
            g(k)
        case _:
            # ruleid: taint-switch-case-python3
            sink(x)
