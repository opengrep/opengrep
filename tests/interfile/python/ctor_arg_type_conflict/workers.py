class AWorker:
    def work(self, data):
        # Reached from unique_flow: UniqueHolder's ctor arg class is the
        # same at every call site, so self.worker is typed AWorker and
        # self.worker.work dispatches here (positive control).
        # ruleid: ctor-arg-type-conflict
        sink(data)


class BWorker:
    def work(self, data):
        # At runtime conflict_flow does reach this sink, but
        # ConflictHolder's callers disagree on the ctor arg class
        # (BWorker in use_a.py, AWorker in use_b.py), so no field type
        # is claimed and no dispatch edge exists — the documented
        # trade-off: a stable miss instead of a file-order coin flip.
        # ok: ctor-arg-type-conflict
        sink(data)
