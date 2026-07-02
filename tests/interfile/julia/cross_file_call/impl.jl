function sink(x)
    println(x)
end

function greet(msg)
    # ruleid: test-cross-file-call
    sink(msg)
end
