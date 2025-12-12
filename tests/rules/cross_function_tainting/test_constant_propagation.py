# Test constant propagation across functions intrafile

def get_literal():
    return "password"

def get_folded():
    return 2 + 2

def get_concat():
    return "pass" + "word"

def use_literal():
    x = get_literal()
    # ruleid: test-constant-propagation
    sink(x)

def use_folded():
    x = get_folded()
    # ruleid: test-constant-propagation
    sink(x)

def use_concat():
    x = get_concat()
    # ruleid: test-constant-propagation
    sink(x)
