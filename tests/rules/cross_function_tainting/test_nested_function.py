def outer():
    def inner():
        return source()

    x = inner()
    # ruleid: test-nested-function-taint
    sink(x)
