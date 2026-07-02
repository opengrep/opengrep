def my_hof_two_of_three(opts, flag):
    if flag:
        cb = opts["a"]
    else:
        cb = opts["b"]
    return cb(opts["data"])

