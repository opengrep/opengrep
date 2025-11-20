-- Comprehensive HOF test for Lua: Custom higher-order functions
-- All of these should detect taint flow from source() to sink()

-- ===== Custom HOF Functions =====
-- Lua doesn't have built-in HOF functions, so we test manual loop implementations

function customMap(arr, callback)
    local result = {}
    for _, item in ipairs(arr) do
        table.insert(result, callback(item))
    end
    return result
end

function customForEach(arr, callback)
    for _, item in ipairs(arr) do
        callback(item)
    end
end

function directCall(callback)
    callback(source())
end

-- ===== Test Cases =====

function test_custom_map()
    local tainted = source()
    local arr = {tainted}
    customMap(arr, function(x)
        -- ruleid: test-hof-taint
        sink(x)
        return x
    end)
end

function test_custom_foreach()
    local tainted = source()
    local arr = {tainted}
    customForEach(arr, function(x)
        -- ruleid: test-hof-taint
        sink(x)
    end)
end

function test_direct_call()
    directCall(function(x)
        -- ruleid: test-hof-taint
        sink(x)
    end)
end

-- ===== Complex Example =====

function getHistory(name, owner)
    local result = source()
    return result
end

function test_original_example()
    local history = getHistory("name", "owner")
    customForEach({history}, function(node)
        local changes = node
        -- ruleid: test-hof-taint
        sink(changes)
    end)
end

-- Stub functions
function source()
    return "tainted"
end

function sink(s)
end
