class Worker:
    def run(self, data):
        # ok: receiver-same-name-local-class
        sink(data)
