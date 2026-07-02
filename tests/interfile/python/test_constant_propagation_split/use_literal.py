def use_literal():
    x = get_literal()
    # ruleid: test-constant-propagation
    sink(x)

