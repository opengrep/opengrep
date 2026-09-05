def test_nested_clean():
    # ok: test-hof-dedup
    sink(wrapper2(propagates, 1))
