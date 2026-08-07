def case_folded_cond():
    arr = [1]
    if len(arr) == 2:
        # ok: test-pruner-python
        sink(source())

