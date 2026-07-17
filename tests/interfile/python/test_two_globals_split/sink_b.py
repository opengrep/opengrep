def sink_b():
    global b
    # ok: test-two-globals
    sink(b)  # This should NOT find anything since b is not tainted

