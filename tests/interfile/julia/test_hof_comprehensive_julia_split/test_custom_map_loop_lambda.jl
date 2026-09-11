function test_custom_map_loop_lambda()
    arr = [source()]
    customMapLoop(arr, x -> begin
        # ruleid: test-hof-taint
        sink(x)
        x
    end)
end
