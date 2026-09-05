def handler():
    return 1


def helper():
    return handler()


def handler():
    return 2


def caller():
    return handler()
