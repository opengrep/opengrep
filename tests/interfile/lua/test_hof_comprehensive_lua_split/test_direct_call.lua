function test_direct_call()
    directCall(function(x)
        -- ruleid: test-hof-taint
        sink(x)
    end)
end
