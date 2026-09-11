def wrap():
    return source()


def main():
    x = wrap()
    # ruleid: test
    sink(x)
