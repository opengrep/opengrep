def chain(p, items):
    x = p
    for item in items:
        x = {"v": "safe", "next": x}
    return x


def tainted_argument(items):
    # ruleid: loop_head_record_built_on_parameter_python
    sink(chain(source(), items)["next"]["next"])


def clean_argument(items):
    # ok: loop_head_record_built_on_parameter_python
    sink(chain("safe", items)["next"]["next"])
