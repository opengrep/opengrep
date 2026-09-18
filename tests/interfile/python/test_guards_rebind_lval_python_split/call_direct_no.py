from C import C
from sink import sink
from source import source
from tagger import tagger

def call_direct_no():
    c = C()
    tagger(c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


