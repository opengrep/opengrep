from foo import process


def source():
    return input()


def main():
    tainted = source()
    process(tainted)


main()
