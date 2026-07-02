def use_folded():
    x = get_folded()
    # ruleid: test-constant-propagation
    sink(x)

