from C import C
from forwarder_shift import forwarder_shift
from sink import sink
from source import source

def call_shift_yes():
    c = C()
    forwarder_shift("d", c, True, source())
    # ruleid: test-guards-rebind-lval
    sink(c.x)
