def outer_gated(p):
    if p["data"][0] != 1:
        return inner_disjoint(p)
    return ""


