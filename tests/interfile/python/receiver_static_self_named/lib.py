class Fire:
    def fire(self, payload):
        # ruleid: receiver-static-self-named
        sink(payload)

class Util:
    @staticmethod
    def emit(self, f: Fire):
        f.fire(source())

class Smoke:
    def smoke(self, payload):
        # ruleid: receiver-static-self-named
        sink(payload)

class Untyped:
    @staticmethod
    def emit_untyped(self, s):
        s.smoke(source())
