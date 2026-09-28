def written(items):
    x = {"v": "safe"}
    for i in items:
        a1 = {"l": x, "r": x}
        a2 = {"l": a1, "r": a1}
        a3 = {"l": a2, "r": a2}
        x = {"l": a3, "r": a3, "v": source()}
    y = {"v": "safe"}
    for i in items:
        b1 = {"l": y, "r": y}
        b2 = {"l": b1, "r": b1}
        b3 = {"l": b2, "r": b2}
        b4 = {"l": b3, "r": b3}
        y = {"l": b4, "r": b4, "w": source()}
    x["l"] = y
    # ruleid: cyclic_record_written_into_field_4_5_python
    sink(x["r"]["l"]["l"]["l"]["v"])
    # ruleid: cyclic_record_written_into_field_4_5_python
    sink(x["l"]["l"]["l"]["l"]["l"]["l"]["w"])
    # ok: cyclic_record_written_into_field_4_5_python
    sink(x["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["v"])
