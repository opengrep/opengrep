# The class main.py actually imports.  Its [run] does not reach a sink.
class Widget:
    def run(self, data, mode=None):
        log(data, mode)
