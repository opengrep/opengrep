g = source()
safe = "constant"


def main():
    # ruleid: module-global-source
    sink(g)


def control():
    # ok: module-global-source
    sink(safe)
