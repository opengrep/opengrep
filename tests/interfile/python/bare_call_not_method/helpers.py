def unrelated(x):
    # ruleid: bare-call-not-method
    sink(x)


def call_it():
    unrelated(source())
