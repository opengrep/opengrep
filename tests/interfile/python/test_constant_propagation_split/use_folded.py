from get_folded import get_folded

def use_folded():
    x = get_folded()
    # ruleid: test-constant-propagation
    sink(x)

