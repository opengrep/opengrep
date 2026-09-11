class Handler:
    def __init__(self, data):
        # ok: nested-binding-shadows-class
        sink(data)


def run():
    def Handler(data):
        # ruleid: nested-binding-shadows-class
        sink(data)

    Handler(source())
