function test_original_example()
    history = getHistory("name", "owner")
    customForEach([history], node -> begin
        changes = node
        # ruleid: test-hof-taint
        sink(changes)
    end)
end
