def query():
    return 1


def other():
    return 2


def tuple_target():
    # Destructuring assignment declares locals just like a single-name
    # assignment: `query` must NOT resolve to the module-level `def query`.
    query, rest = make(), 0
    sink(query)


def augmented_target():
    # An augmented assignment to an undeclared name is a local write
    # (Python raises UnboundLocalError at runtime, so it is certainly not
    # a reference to the module function).
    other = make()
    other += "x"
    sink(other)
