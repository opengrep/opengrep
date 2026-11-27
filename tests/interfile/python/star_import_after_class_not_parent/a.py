class Base:
    def f(self, x):
        # ruleid: star-import-after-class-not-parent
        sink(x)
