from sink import sink
from source import source

def case_pre_existing():
    a = source()
    if False:
        # ok: test-pruner-python
        sink(a)

