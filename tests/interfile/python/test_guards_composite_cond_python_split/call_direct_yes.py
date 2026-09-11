from handler import handler
from sink import sink
from source import source

def call_direct_yes():
    # ruleid: test-guards-composite-cond
    sink(handler(2, source()))


