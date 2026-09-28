def joined(c, items):
    x = {"v": "safe"}
    if c:
        for i in items:
            a1 = {"l": x, "r": x}
            a2 = {"l": a1, "r": a1}
            x = {"l": a2, "r": a2, "v": source()}
    else:
        for i in items:
            b1 = {"l": x, "r": x}
            b2 = {"l": b1, "r": b1}
            b3 = {"l": b2, "r": b2}
            b4 = {"l": b3, "r": b3}
            x = {"l": b4, "r": b4, "w": source()}
    # ruleid: branch_join_of_cyclic_records_3_5_python
    sink(x["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["v"])
    # ruleid: branch_join_of_cyclic_records_3_5_python
    sink(x["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["w"])
    # ok: branch_join_of_cyclic_records_3_5_python
    sink(x["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["l"]["v"])
