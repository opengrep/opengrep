# A field written after a write through an arbitrary key holds its own value;
# a write through an arbitrary key after it may write it.
def f(k):
    d = {}
    d[k] = source()
    d["safe"] = "ok"
    # ok: python_field_written_after_arbitrary_key
    sink(d["safe"])
    # ruleid: python_field_written_after_arbitrary_key
    sink(d["other"])

def g(k):
    d = {}
    d["safe"] = "ok"
    d[k] = source()
    # ruleid: python_field_written_after_arbitrary_key
    sink(d["safe"])
