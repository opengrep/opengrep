def joined_below_cycle(c, items):
    x = {"next": None, "v": "safe", "t": source()}
    for i in items:
        x = {"next": x, "v": "safe", "t": source()}
    if c:
        x = {"next": {"v": source()}, "v": "safe"}
    # ruleid: field_below_cycle_on_one_branch_python
    sink(x["next"]["v"])
    # ok: field_below_cycle_on_one_branch_python
    sink(x["next"]["next"]["v"])
