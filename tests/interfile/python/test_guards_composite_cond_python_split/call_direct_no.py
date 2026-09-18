from handler import handler
from sink import sink
from source import source

def call_direct_no():
    # ok: test-guards-composite-cond
    sink(handler(9, source()))


