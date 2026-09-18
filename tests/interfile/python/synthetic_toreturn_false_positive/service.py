class Service:
    def __init__(self):
        self.data = None

    def configure(self, value):
        self.data = value
        return "ok"
