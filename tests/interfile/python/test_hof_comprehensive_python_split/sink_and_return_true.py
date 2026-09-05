def sink_and_return_true(x):
    # ruleid: test-hof-taint
    sink(x)
    return True

