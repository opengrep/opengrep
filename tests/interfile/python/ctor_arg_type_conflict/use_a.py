from holder import UniqueHolder, ConflictHolder

from workers import AWorker, BWorker


def unique_flow():
    h = UniqueHolder(AWorker())
    h.run(taint())


def conflict_flow():
    h = ConflictHolder(BWorker())
    h.run(taint())
