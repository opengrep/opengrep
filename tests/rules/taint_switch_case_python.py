def later_mapping_case_is_reachable(m):
    x = taint_source()
    match m:
        case {"a": a}:
            g(a)
        case {"b": _}:
            # ruleid: taint-switch-case-python
            sink(x)


def cases_do_not_fall_through(k):
    y = 0
    match k:
        case 1:
            y = taint_source()
        case _:
            # ok: taint-switch-case-python
            sink(y)
