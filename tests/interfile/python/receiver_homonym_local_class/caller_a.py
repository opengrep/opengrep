from base_a import BaseA


class Worker(BaseA):
    pass


def go_a():
    Worker().run(source())
