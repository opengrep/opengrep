function customMap(arr, callback)
    local result = {}
    for _, item in ipairs(arr) do
        table.insert(result, callback(item))
    end
    return result
end
