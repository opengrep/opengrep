import apply


class Runner:
    def handle(self, data):
        # ruleid: callback-self-method
        sink(data)

    def go(self):
        apply.run(self.handle, source())
