from sink import sink
from source import source

def case_else_dead():
    if True:
        x = ""
    else:
        # ok: test-pruner-python
        sink(source())

