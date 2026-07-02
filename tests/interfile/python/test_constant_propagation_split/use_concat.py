def use_concat():
    x = get_concat()
    # ruleid: test-constant-propagation
    sink(x)
