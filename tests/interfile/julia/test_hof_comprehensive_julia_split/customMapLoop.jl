function customMapLoop(arr, callback)
    result = []
    for item in arr
        push!(result, callback(item))
    end
    return result
end
