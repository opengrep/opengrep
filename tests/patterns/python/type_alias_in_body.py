def configure():
    # ERROR:
    type Config = dict[str, str]
    return {}

class Service:
    # ERROR:
    type ID = int

    def run(self):
        pass

type = 5
x = type(42)
