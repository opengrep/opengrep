def main():
    taint_a()  # Taint global a
    sink_b()   # Try to sink global b (should fail)