from f import f
from sink import sink
from source import source

def calls():
    # ok: test-guard-effect-fuse
    sink(f(0, source()))
    # ruleid: test-guard-effect-fuse
    sink(f(1, source()))
