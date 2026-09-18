from holder import ConflictHolder

from workers import AWorker


def other_caller():
    # Exists only to make ConflictHolder's ctor arg classes conflict.
    ConflictHolder(AWorker())
