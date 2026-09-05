def my_hof_same(opts, flag):
    if flag:
        cb = opts["a"]
    else:
        cb = opts["a"]
    return cb(opts["data"])

