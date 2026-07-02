function sink(x)
    println(x)
end

function emit(data)
    # ok: test-sanitiser
    sink(data)
end
