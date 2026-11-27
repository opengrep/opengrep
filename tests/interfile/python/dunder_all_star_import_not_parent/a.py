__all__ = ["other"]


def other():
    pass


class Base:
    def f(self, x):
        # ok: dunder-all-star-import-not-parent
        sink(x)
