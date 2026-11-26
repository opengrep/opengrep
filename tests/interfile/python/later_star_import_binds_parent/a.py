class Base:
    def f(self, x):
        # ok: later-star-import-binds-parent
        sink(x)
