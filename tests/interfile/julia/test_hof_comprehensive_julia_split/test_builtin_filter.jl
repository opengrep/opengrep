function test_builtin_filter()
    arr = [source()]
    filter(x -> begin
        # ruleid: test-hof-taint
        sink(x)
        true
    end, arr)
end
