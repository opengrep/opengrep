class Store:
    def __init__(self, data):
        # ruleid: ctor-call-in-method
        sink(data)
