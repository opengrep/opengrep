def joined(items, flag):
    x = {"a": "safe", "b": source(), "next": None}
    for item in items:
        x = {"a": "safe", "b": source(), "next": x}
    if flag:
        x = {"a": "safe", "next": source()}
    # ruleid: back_reference_joined_with_tainted_value_python
    sink(x["next"]["a"])


def not_joined(items, flag):
    x = {"a": "safe", "b": source(), "next": None}
    for item in items:
        x = {"a": "safe", "b": source(), "next": x}
    if flag:
        x = {"a": "safe", "next": "safe", "b": source()}
    # ok: back_reference_joined_with_tainted_value_python
    sink(x["next"]["a"])
