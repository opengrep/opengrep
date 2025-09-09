x = None
y = None

def taint_y():
    global y
    y = source1("taint")  # This taints y

def sink_x():
    global x
    sink(x)  # This should NOT find anything since x is not tainted

def main():
    taint_y()  # Taint y
    sink_x()   # Try to sink x (should fail)
