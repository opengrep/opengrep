function customForEach(arr, callback)
    for _, item in ipairs(arr) do
        callback(item)
    end
end
