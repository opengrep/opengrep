def case_else_dead():
    if True:
        x = ""
    else:
        # ok: test-pruner-python
        sink(source())

