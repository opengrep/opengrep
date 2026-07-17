class Decoy:
    def process(self, data):
        # Same-named method so `process` is not unique project-wide:
        # w.process() can only resolve through Worker's (absent) MRO,
        # isolating the parent-resolution heuristic.
        return data
