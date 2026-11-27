class Base:
    def f(self, x):
        # ok: own-class-parent-ahead-of-star-import
        sink(x)
