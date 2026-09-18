class Emitter:
    def emit(self, payload):
        # ruleid: receiver-not-named-self
        sink(payload)

class Widget:
    def render(me, e):
        e.emit(source())
