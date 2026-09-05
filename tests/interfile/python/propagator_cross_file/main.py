from helper import stash


def run():
    box = []
    stash(box, source())
    # ruleid: propagator-cross-file
    sink(box)
