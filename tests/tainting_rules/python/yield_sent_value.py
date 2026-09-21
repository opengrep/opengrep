def gen():
    x = yield source()
    # ok: yield_sent_value
    sink(x)
