def written_into_cycle(items):
    x = {"next": None, "v": "safe", "t": source()}
    for i in items:
        x = {"next": x, "v": "safe", "t": source()}
    x["next"]["v"] = source()
    # ruleid: write_into_record_of_cycle_python
    sink(x["next"]["v"])
    # todook: write_into_record_of_cycle_python
    sink(x["v"])
    # ok: write_into_record_of_cycle_python
    sink(x["w"])
