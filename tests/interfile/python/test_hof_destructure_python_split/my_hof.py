def my_hof(opts):
    cb = opts["cb"]
    data = opts["data"]
    return cb(data)


