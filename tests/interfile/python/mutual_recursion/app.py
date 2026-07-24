from lib import p


def sink(x):
    pass


def main():
    # p(0) is called with an UNtainted argument; the finding depends entirely
    # on p's return signature carrying the source injected inside the cycle
    # (q -> r(source())), which needs the SCC fixpoint to converge.
    # ruleid: mutual-recursion
    sink(p(0))
