from hof_lib import apply_fn


def source():
    return ""


def run():
    # A callback defined inside the function that passes it: the nested
    # definition is a local name, and still a function.
    def report(v):
        # ruleid: hof-nested-callback
        sink(v)

    apply_fn(report, source())
