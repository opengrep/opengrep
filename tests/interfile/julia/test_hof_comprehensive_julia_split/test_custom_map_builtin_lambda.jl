function test_custom_map_builtin_lambda()
    arr = [source()]
    customMapBuiltin(arr, x -> begin
        # ruleid: test-hof-taint
        sink(x)
        x
    end)
end
