def my_hof_2way(opts, flag):
    if flag:
        cb = opts["a"]
    else:
        cb = opts["b"]
    return cb(opts["data"])

