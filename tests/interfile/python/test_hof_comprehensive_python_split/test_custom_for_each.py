def test_custom_for_each():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_for_each(arr, lambda x: (
        sink(x)
    ))

