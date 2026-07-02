function main()
    local taintedInput = getTaintedData()
    local result = processData(taintedInput)
    return result
end
