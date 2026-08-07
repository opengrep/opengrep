function sink(x)
    println(x)
end

function leak(x)
    # ruleid: test-passthrough
    sink(x)
end
