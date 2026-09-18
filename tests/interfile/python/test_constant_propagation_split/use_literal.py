from get_literal import get_literal

def use_literal():
    x = get_literal()
    # ruleid: test-constant-propagation
    sink(x)

