def sink_b():
    global b  
    sink(b)  # This should NOT find anything since b is not tainted

