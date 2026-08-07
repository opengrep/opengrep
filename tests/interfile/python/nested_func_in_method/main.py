from sinks import leak_method, leak_nested, leak_top


class Handler:
    # The method itself DOES have an implicit receiver: [data] is parameter 0
    # of the call, [self] takes no call argument.
    def handle(self, data):
        leak_method(data)

    def run(self):
        # [inner] is nested inside a method, so it is NOT a method: [value] is
        # its first *real* parameter and must not be stripped as a receiver.
        def inner(value):
            leak_nested(value)

        inner(source())


# Byte-identical nesting at module level, as a control.
def top():
    def inner2(value):
        leak_top(value)

    inner2(source())


def drive():
    Handler().handle(source())
