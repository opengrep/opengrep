def float_equals_int():
    x = taint_source()
    k = 1.0
    if k == 1:
        # ruleid: int-float-equality
        sink(x)


def int_not_equals_float():
    x = taint_source()
    k = 1
    if k != 1.0:
        # ok: int-float-equality
        sink(x)


def float_matches_int_case():
    x = taint_source()
    k = 1.0
    match k:
        case 1:
            # ruleid: int-float-equality
            sink(x)


def float_differs_from_int():
    x = taint_source()
    k = 1.5
    if k == 1:
        # ok: int-float-equality
        sink(x)
