from sink import sink
from source import source

def case_fresh_in_branch():
    if False:
        # ok: test-pruner-python
        sink(source())

