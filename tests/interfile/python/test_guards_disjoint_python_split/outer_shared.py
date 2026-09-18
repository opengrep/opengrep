from inner_shared import inner_shared

def outer_shared(p):
    return inner_shared(p)


