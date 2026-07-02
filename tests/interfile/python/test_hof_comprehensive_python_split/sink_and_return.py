def sink_and_return(x):
    # ruleid: test-hof-taint
    sink(x)
    return x

