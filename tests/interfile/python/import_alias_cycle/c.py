from a import f, x


class Handler:
    def handle(self, v):
        x(v)
        # ruleid: import-alias-cycle
        f(v)


def run():
    Handler().handle(source())
