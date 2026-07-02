def test_direct_call():
    # ruleid: test-hof-taint
    direct_call(lambda x: (
        sink(x)
    ))

