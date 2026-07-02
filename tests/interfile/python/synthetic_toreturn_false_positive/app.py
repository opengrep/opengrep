from service import Service

def test():
    s = Service()
    result = s.configure(source())
    # ok: test-setter-false-positive
    sink(result)
