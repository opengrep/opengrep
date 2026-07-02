function test_custom_foreach()
    local tainted = source()
    local arr = {tainted}
    customForEach(arr, function(x)
        -- ruleid: test-hof-taint
        sink(x)
    end)
end
