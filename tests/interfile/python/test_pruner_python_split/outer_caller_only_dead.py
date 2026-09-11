from inner_only_dead import inner_only_dead
from sink import sink

def outer_caller_only_dead():
    # ok: test-pruner-python
    sink(inner_only_dead())

