def test_nested_tainted():
    # ruleid: test-hof-dedup
    sink(wrapper2(propagates, source()))


