function sink(x)
    println(x)
end

function handle(msg)
    # ruleid: test-project-scope-two-files
    sink(msg)
end
