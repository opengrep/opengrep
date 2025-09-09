function getTaintedData()
    return source()
end

function processData(data)
    # ruleid: simple_julia_taint
    return sink(data)
end

function main()
    taintedInput = getTaintedData()
    result = processData(taintedInput)
    return result
end

main()