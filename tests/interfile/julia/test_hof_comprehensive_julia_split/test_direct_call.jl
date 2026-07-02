function test_direct_call()
    directCall(x -> begin
        # ruleid: test-hof-taint
        sink(x)
    end)
end
