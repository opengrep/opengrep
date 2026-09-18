from sink import sink
from source import source
from wrapper import wrapper

def call_forward_no():
    # ok: test-guards-composite-cond
    sink(wrapper(9, source()))


