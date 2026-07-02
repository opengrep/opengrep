def my_hof_3way(opts, mode):
    if mode == "x":
        cb = opts["x"]
    elif mode == "y":
        cb = opts["y"]
    else:
        cb = opts["z"]
    return cb(opts["data"])

