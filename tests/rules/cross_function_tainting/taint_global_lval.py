x = None

def foo_lval():
    global x
    x = source1("taint")

def intermediate():
    foo_lval()    # Taint the global through intermediate call

def do_sink():
    global x
    # ruleid: test-global-lval-taint
    sink(x)

def main():
    intermediate()  # Call intermediate function that taints global x
    do_sink()       # Sink through global variable access