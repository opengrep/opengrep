function test_custom_foreach()
    arr = [source()]
    customForEach(arr, x -> begin
        # ruleid: test-hof-taint
        sink(x)
    end)
end
