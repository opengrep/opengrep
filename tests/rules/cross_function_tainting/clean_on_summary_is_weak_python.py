def chain(items):
    rec = {"v": source(), "next": None}
    for item in items:
        rec = {"v": "safe", "b": source(), "next": rec}
    rec["v"] = "safe"
    # ruleid: clean_on_summary_is_weak_python
    sink(rec["next"]["v"])


def single():
    rec = {"v": source(), "next": None}
    rec = {"v": "safe", "b": source(), "next": rec}
    rec["next"]["v"] = "safe"
    # ok: clean_on_summary_is_weak_python
    sink(rec["next"]["v"])
