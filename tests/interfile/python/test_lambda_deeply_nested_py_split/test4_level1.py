def test4_level1(x):
    # ruleid: test-lambda-deeply-nested-py
    level3 = lambda: sink(x)
    level2 = lambda: level3()
    level2()

