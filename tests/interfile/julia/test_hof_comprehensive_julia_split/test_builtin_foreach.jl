function test_builtin_foreach()
    arr = [source()]
    foreach(x -> begin
        # ruleid: test-hof-taint
        sink(x)
    end, arr)
end
