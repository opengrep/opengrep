from custom_map_builtin import custom_map_builtin

def test_custom_map_builtin_lambda():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_map_builtin(arr, lambda x: (
        sink(x)
    ))

