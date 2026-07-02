function test_builtin_map()
    arr = [source()]
    map(x -> begin
        # ruleid: test-hof-taint
        sink(x)
        x
    end, arr)
end
