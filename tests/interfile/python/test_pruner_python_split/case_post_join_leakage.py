from sink import sink
from source import source

def case_post_join_leakage():
    x = "safe"
    if False:
        x = source()
    # ok: test-pruner-python
    sink(x)

