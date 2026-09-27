def tainted():
    return source()


def safe():
    return "safe"


def functions(items):
    x = {"f": safe, "next": None}
    for item in items:
        x = {"f": tainted, "next": x}
    # ruleid: loop_head_record_of_functions_python
    sink(x["next"]["f"]())


def safe_functions(items):
    x = {"f": safe, "next": None}
    for item in items:
        x = {"f": safe, "next": x}
    # ok: loop_head_record_of_functions_python
    sink(x["next"]["f"]())
