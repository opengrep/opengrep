def case_nested_dead():
    if False:
        if False:
            # ok: test-pruner-python
            sink(source())

