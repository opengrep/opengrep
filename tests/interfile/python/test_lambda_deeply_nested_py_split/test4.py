def test4():
    x = source()
    # ruleid: test-lambda-deeply-nested-py
    level3 = lambda: sink(x)
    level2 = lambda: level3()
    level1 = lambda: level2()
    level1()

