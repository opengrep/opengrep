from C import C
from sink import sink
from source import source
from tagger import tagger

def call_direct_yes():
    c = C()
    tagger(c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)


