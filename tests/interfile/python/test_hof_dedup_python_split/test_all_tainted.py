def test_all_tainted():
    # ruleid: test-hof-dedup
    sink(helper(propagates, source(), source(), source()))


