from custom_map_loop import custom_map_loop

def test_custom_map_loop_lambda():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_map_loop(arr, lambda x: (
        sink(x)
    ))

