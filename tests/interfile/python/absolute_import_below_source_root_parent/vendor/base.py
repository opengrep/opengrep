class Base:
    def f(self, x):
        # ok: absolute-import-below-source-root-parent
        sink(x)
