# Imports the aliased name `run` from the package `mypkg`.
from mypkg import run


def source():
    return input()


def main():
    tainted = source()
    run(tainted)


main()
