from sink import sink
from source import source
from wrapper import wrapper

def call_forward_yes():
    # ruleid: test-guards-hof-return
    sink(wrapper(True, source()))
