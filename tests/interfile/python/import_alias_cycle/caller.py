from a import f, x
from b import y


def run():
    x(source())
    y(source())
    # ruleid: import-alias-cycle
    f(source())
