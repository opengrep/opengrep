def outer_caller_only_dead():
    # ok: test-pruner-python
    sink(inner_only_dead())

