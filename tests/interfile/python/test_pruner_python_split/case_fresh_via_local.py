from sink import sink
from source import source

def case_fresh_via_local():
    if False:
        x = source()
        # ok: test-pruner-python
        sink(x)

