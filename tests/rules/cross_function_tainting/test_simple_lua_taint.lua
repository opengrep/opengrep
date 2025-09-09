function getTaintedData()
    return source()
end

function processData(data)
    -- ruleid: simple_lua_taint
    return sink(data)
end

function main()
    local taintedInput = getTaintedData()
    local result = processData(taintedInput)
    return result
end

main()