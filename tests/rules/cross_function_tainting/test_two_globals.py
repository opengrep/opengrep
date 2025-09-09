a = None
b = None

def taint_a():
    global a
    a = source1("taint")  # Taint global a

def sink_b():
    global b  
    sink(b)  # This should NOT find anything since b is not tainted

def main():
    taint_a()  # Taint global a
    sink_b()   # Try to sink global b (should fail)