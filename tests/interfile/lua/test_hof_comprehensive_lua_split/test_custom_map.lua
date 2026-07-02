function test_custom_map()
    local tainted = source()
    local arr = {tainted}
    customMap(arr, function(x)
        -- ruleid: test-hof-taint
        sink(x)
        return x
    end)
end
