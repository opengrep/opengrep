# Test: Deeply nested lambdas (3 levels)
def test4():
    x = source()
    # ruleid: test-lambda-deeply-nested-py
    level3 = lambda: sink(x)
    level2 = lambda: level3()
    level1 = lambda: level2()
    level1()

# Test: Deeply nested lambdas split across functions
def test4_level1(x):
    # ruleid: test-lambda-deeply-nested-py
    level3 = lambda: sink(x)
    level2 = lambda: level3()
    level2()

def test4_caller():
    x = source()
    test4_level1(x)

def source():
    return "tainted"

def sink(x):
    pass
