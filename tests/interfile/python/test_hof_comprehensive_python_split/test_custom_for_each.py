from custom_for_each import custom_for_each

def test_custom_for_each():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_for_each(arr, lambda x: (
        sink(x)
    ))

