from sink_x import sink_x
from taint_y import taint_y

def main():
    taint_y()  # Taint y
    sink_x()   # Try to sink x (should fail)
