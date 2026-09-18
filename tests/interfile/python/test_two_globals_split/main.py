from sink_b import sink_b
from taint_a import taint_a

def main():
    taint_a()  # Taint global a
    sink_b()   # Try to sink global b (should fail)