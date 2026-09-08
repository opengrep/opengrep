class Fire:
    def fire(self, payload):
        # ruleid: receiver-static-self-named
        sink(payload)

class Util:
    @staticmethod
    def emit(self, f):
        f.fire(source())
