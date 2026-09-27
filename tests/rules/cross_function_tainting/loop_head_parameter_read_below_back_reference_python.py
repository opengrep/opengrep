def chain(p, items):
    x = p
    for item in items:
        x = {"h": "safe", "next": x}
    return x


def a(items):
    # ruleid: loop_head_parameter_read_below_back_reference_python
    sink(chain({"next": {"h": source()}}, items)["next"]["next"]["h"])


def b(items):
    # ruleid: loop_head_parameter_read_below_back_reference_python
    sink(chain({"h": source()}, items)["next"]["h"])


def c(items):
    # ok: loop_head_parameter_read_below_back_reference_python
    sink(chain({"next": {"h": "safe"}}, items)["next"]["next"]["h"])
