class Helper:
    def run(self, x):
        # ruleid: python_receiver_untyped_field
        sink(x)

    def safe(self, x):
        # ok: python_receiver_untyped_field
        sink("constant")


class Main:
    def __init__(self):
        self.h = Helper()

    def go(self):
        self.h.run(source())
        self.h.safe(source())
