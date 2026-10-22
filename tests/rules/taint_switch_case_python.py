def cases_do_not_fall_through(k):
    y = 0
    match k:
        case 1:
            y = taint_source()
        case _:
            # ok: taint-switch-case-python
            sink(y)


def break_leaves_the_loop(ks):
    y = 0
    for k in ks:
        match k:
            case 1:
                y = taint_source()
                break
            case _:
                pass
        y = 0
    # ruleid: taint-switch-case-python
    sink(y)


def break_skips_the_rest_of_the_loop(ks):
    for k in ks:
        y = 0
        match k:
            case 1:
                y = taint_source()
                break
            case _:
                pass
        # ok: taint-switch-case-python
        sink(y)
