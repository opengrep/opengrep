from sink import sink
from source import source
from wrapper import wrapper

def call_forward_yes():
    # ruleid: test-guards-composite-cond
    sink(wrapper(2, source()))
