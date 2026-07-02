def make_writer(target):
    def writer(v):
        target[0] = v
    return writer

