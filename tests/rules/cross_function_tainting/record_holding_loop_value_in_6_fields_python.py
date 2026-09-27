def deep(items):
    x = {}
    for i in items:
        x = {"a": {"f0": {"g": x, "v": source()}, "f1": {"g": x, "v": source()}, "f2": {"g": x, "v": source()}, "f3": {"g": x, "v": source()}, "f4": {"g": x, "v": source()}, "f5": {"g": x, "v": source()}}}
    # ruleid: record_holding_loop_value_in_6_fields_python
    sink(x)
