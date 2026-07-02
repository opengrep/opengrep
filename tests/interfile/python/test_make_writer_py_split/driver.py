def driver():
    out = []
    w = make_writer(out)
    w(source())
    # ruleid: test-make-writer-py
    sink(out)
