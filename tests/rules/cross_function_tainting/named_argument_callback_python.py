def second_result(first, second):
    return second()


def clean_second_by_name():
    tainted = lambda: source()
    clean = lambda: "safe"
    # ok: named_argument_callback_python
    sink(second_result(second=clean, first=tainted))


def tainted_second_by_name():
    tainted = lambda: source()
    clean = lambda: "safe"
    # ruleid: named_argument_callback_python
    sink(second_result(second=tainted, first=clean))
