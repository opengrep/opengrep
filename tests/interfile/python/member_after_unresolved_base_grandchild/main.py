from leaf import Leaf, OwnLeaf


def run():
    Leaf().handle(source())


def run_own():
    OwnLeaf().handle(source())
