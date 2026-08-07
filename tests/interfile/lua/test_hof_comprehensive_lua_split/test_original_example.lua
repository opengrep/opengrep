function test_original_example()
    local history = getHistory("name", "owner")
    customForEach({history}, function(node)
        local changes = node
        -- ruleid: test-hof-taint
        sink(changes)
    end)
end
