class Base:
    def f(self, x):
        # ok: star-import-after-class-not-parent
        sink(x)
