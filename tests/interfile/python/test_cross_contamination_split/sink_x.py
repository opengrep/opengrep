def sink_x():
    global x
    sink(x)  # This should NOT find anything since x is not tainted

