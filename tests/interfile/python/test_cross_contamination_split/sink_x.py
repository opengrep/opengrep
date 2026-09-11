def sink_x():
    global x
    # ok: test-cross-contamination
    sink(x)  # This should NOT find anything since x is not tainted

