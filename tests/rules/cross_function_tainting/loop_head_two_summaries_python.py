def two_chains(items):
    x = {"a": None, "b": None}
    for item in items:
        x = {"a": {"v": source(), "next": x["a"]}, "b": {"w": "safe", "next": x["b"]}}
    # ruleid: loop_head_two_summaries_python
    sink(x["a"]["next"]["v"])
    # ok: loop_head_two_summaries_python
    sink(x["b"]["next"]["w"])
