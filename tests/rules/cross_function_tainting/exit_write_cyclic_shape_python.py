def build(p, items):
    for x in items:
        p["next"] = {"v": source(), "w": "safe", "next": p["next"]}


def first_node(items):
    a = {"next": None}
    build(a, items)
    # ruleid: exit_write_cyclic_shape_python
    sink(a["next"]["v"])


def second_node(items):
    a = {"next": None}
    build(a, items)
    # ruleid: exit_write_cyclic_shape_python
    sink(a["next"]["next"]["v"])


def third_node(items):
    a = {"next": None}
    build(a, items)
    # ruleid: exit_write_cyclic_shape_python
    sink(a["next"]["next"]["next"]["v"])


def clean_field(items):
    a = {"next": None}
    build(a, items)
    # ok: exit_write_cyclic_shape_python
    sink(a["next"]["next"]["w"])


def two_levels_param(p, items):
    for i in items:
        p["n"] = {"a": {"b": p["n"], "v": source()}}


def caller_two_levels(items):
    q = {"n": None}
    two_levels_param(q, items)
    # ruleid: exit_write_cyclic_shape_python
    sink(q["n"]["a"]["b"]["a"]["v"])
