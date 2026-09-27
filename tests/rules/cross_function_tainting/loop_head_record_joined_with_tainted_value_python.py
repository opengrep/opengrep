def tainted_start(items):
    x = source()
    for item in items:
        x = {"a": "safe", "next": x}
    # ruleid: loop_head_record_joined_with_tainted_value_python
    sink(x["next"]["a"])


def clean_start(items):
    x = {"a": "safe", "b": source(), "next": None}
    for item in items:
        x = {"a": "safe", "b": source(), "next": x}
    # ok: loop_head_record_joined_with_tainted_value_python
    sink(x["next"]["a"])
