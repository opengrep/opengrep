class Base:
    def f(self, x):
        # ok: package-init-star-reexport-parent
        sink(x)
