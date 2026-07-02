def test_first_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, source(), 1, 1))


