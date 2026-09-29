class Handler:
    def run(self, v):
        # ruleid: method-reference-callback
        sink(v)


class Runner:
    def handle(self, data):
        # ruleid: method-reference-callback
        sink(data)

    def go(self):
        apply(self.handle, source())


def _tmp_handler(v):
    # ruleid: method-reference-callback
    sink(v)


def apply(cb, x):
    cb(x)


def main():
    h = Handler()
    apply(h.run, source())


def tmp_prefixed_function():
    apply(_tmp_handler, source())
