def test_custom_map_loop_lambda():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_map_loop(arr, lambda x: (
        sink(x)
    ))

