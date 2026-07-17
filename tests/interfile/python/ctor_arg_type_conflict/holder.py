class UniqueHolder:
    def __init__(self, worker):
        # Every caller passes an AWorker: the field is typed from the
        # unique caller-supplied class.
        self.worker = worker

    def run(self, data):
        self.worker.work(data)


class ConflictHolder:
    def __init__(self, worker):
        # Callers disagree (BWorker in use_a.py, AWorker in use_b.py):
        # no type is claimed for the field.
        self.worker = worker

    def run(self, data):
        self.worker.work(data)
