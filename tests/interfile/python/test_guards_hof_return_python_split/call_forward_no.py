from sink import sink
from source import source
from wrapper import wrapper

def call_forward_no():
    # ok: test-guards-hof-return
    sink(wrapper(False, source()))


