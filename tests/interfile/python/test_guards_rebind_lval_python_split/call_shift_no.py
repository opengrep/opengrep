from C import C
from forwarder_shift import forwarder_shift
from sink import sink
from source import source

def call_shift_no():
    c = C()
    forwarder_shift("d", c, False, source())
    # ok: test-guards-rebind-lval
    sink(c.x)


