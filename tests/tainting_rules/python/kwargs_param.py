def foo(**kwargs):
    # ruleid: taint
    sink(kwargs)

# Regular positional param — not matched by the **$PARAM source pattern.
def bar(x):
    # ok: taint
    sink(x)
