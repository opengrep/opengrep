from apply import apply
from handler import handler
from sink import sink
from source import source

def call_literal_no():
    # ok: test-guards-hof-return
    sink(apply(handler, False, source()))


