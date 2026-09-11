class Runner:
    @staticmethod
    def run(data):
        keep(data)


from mod_b import Runner


def go_a():
    Runner.run(source())
