class C:
    # 's' is the receiver (whatever it is named); the tainted call argument
    # must map to 'x', not to the receiver.
    def m(s, x):
        # ruleid: nonstandard-receiver
        sink(x)


def main():
    c = C()
    c.m(source())
