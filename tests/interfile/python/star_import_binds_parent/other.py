class Base:
    def f(self, x):
        # ok: star-import-binds-parent
        sink(x)
