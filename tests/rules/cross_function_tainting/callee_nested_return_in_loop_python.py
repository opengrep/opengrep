def load():
    return {"user": {"name": source()}, "name": "fixed"}


def nested_return(items):
    cfg = None
    for item in items:
        cfg = load()
    # ok: callee_nested_return_in_loop_python
    sink(cfg["name"])
    # ruleid: callee_nested_return_in_loop_python
    sink(cfg["user"]["name"])


def chain(items):
    node = {"v": source(), "next": None}
    for item in items:
        node = {"v": "safe", "next": node}
    return node


def recursive_return(items):
    cfg = None
    for item in items:
        cfg = chain(items)
    # ruleid: callee_nested_return_in_loop_python
    sink(cfg["next"]["next"]["v"])
