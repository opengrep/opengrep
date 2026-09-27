def tainted_value(items):
    x = {"c": "safe", "next": None}
    for k in items:
        x = {"a": "safe", "next": x}
        x[k] = {"v": source()}
    # ruleid: loop_head_record_with_any_key_python
    sink(x["c"]["v"])


def safe_value(items):
    x = {"c": "safe", "next": None}
    for k in items:
        x = {"a": "safe", "b": source(), "next": x}
        x[k] = {"v": "safe"}
    # ok: loop_head_record_with_any_key_python
    sink(x["c"]["v"])
