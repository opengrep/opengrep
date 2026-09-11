class Base:
    def __init__(self, value):
        self.value = value

    def process(self):
        # ruleid: class-redefinition-last-wins
        sink(self.value)


def sink(x):
    print(x)
