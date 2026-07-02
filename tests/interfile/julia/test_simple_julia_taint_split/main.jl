function main()
    taintedInput = getTaintedData()
    result = processData(taintedInput)
    return result
end
