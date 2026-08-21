class Store:
    def __init__(self, data):
        self.data = data

    def send(self):
        # ruleid: test-constructor-taint
        sink(self.data)

def sink(x):
    print(x)
