from apply import apply
from handler import handler
from sink import sink
from source import source

def call_literal_yes():
    # ruleid: test-guards-hof-return
    sink(apply(handler, True, source()))


