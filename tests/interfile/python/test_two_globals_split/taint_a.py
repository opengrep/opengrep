def taint_a():
    global a
    a = source1("taint")  # Taint global a

