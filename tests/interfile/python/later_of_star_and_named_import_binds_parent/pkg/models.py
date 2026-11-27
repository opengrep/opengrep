class Base:
    def f(self, x):
        # ruleid: later-of-star-and-named-import-binds-parent
        sink(x)


class Handler:
    def f(self, x):
        # ok: later-of-star-and-named-import-binds-parent
        sink(x)
