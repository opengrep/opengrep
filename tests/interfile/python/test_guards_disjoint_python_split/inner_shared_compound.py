def inner_shared_compound(opts):
    x = source()
    if len(opts["data"]) == 2 or opts["data"][0] == 1:
        return x
    return x


