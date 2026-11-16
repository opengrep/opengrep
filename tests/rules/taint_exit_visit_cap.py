# No loop here. The exit node joins six return paths, so a visit limit on it
# drops the later paths and with them the update of obj.data.
def fill(obj, k):
    if k == 1:
        return
    if k == 2:
        return
    if k == 3:
        return
    if k == 4:
        return
    if k == 5:
        return
    obj.data = source()


def fill_clean(obj, k):
    if k == 1:
        return
    if k == 2:
        return
    if k == 3:
        return
    if k == 4:
        return
    if k == 5:
        return
    obj.data = "safe"


def caller(o, k):
    fill(o, k)
    # ruleid: taint_exit_visit_cap
    sink(o.data)


def caller_clean(o, k):
    fill_clean(o, k)
    # ok: taint_exit_visit_cap
    sink(o.data)
