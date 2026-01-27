# Comprehensive HOF test for Julia: Custom and built-in higher-order functions
# All of these should detect taint flow from source() to sink()

# ===== Custom HOF Functions =====

# Manual loop implementation
function customMapLoop(arr, callback)
    result = []
    for item in arr
        push!(result, callback(item))
    end
    return result
end

# Delegates to built-in (tests ToSinkInCall propagation)
function customMapBuiltin(arr, callback)
    return map(callback, arr)
end

function customForEach(arr, callback)
    for item in arr
        callback(item)
    end
end

function directCall(callback)
    callback(source())
end

# ===== Test Cases =====

# Named function for testing
function process(x)
    # ruleid: test-hof-taint
    sink(x)
    return x
end

# Test custom HOF with manual loop + lambda
function test_custom_map_loop_lambda()
    arr = [source()]
    customMapLoop(arr, x -> begin
        # ruleid: test-hof-taint
        sink(x)
        x
    end)
end

# Test custom HOF with manual loop + named function
function test_custom_map_loop_named()
    arr = [source()]
    customMapLoop(arr, process)
end

# Test custom HOF delegating to built-in + lambda
function test_custom_map_builtin_lambda()
    arr = [source()]
    customMapBuiltin(arr, x -> begin
        # ruleid: test-hof-taint
        sink(x)
        x
    end)
end

# Test custom HOF delegating to built-in + named function
function test_custom_map_builtin_named()
    arr = [source()]
    customMapBuiltin(arr, process)
end

function test_custom_foreach()
    arr = [source()]
    customForEach(arr, x -> begin
        # ruleid: test-hof-taint
        sink(x)
    end)
end

function test_direct_call()
    directCall(x -> begin
        # ruleid: test-hof-taint
        sink(x)
    end)
end

# ===== Built-in functions =====

function test_builtin_map()
    arr = [source()]
    map(x -> begin
        # ruleid: test-hof-taint
        sink(x)
        x
    end, arr)
end

function test_builtin_foreach()
    arr = [source()]
    foreach(x -> begin
        # ruleid: test-hof-taint
        sink(x)
    end, arr)
end

function test_builtin_filter()
    arr = [source()]
    filter(x -> begin
        # ruleid: test-hof-taint
        sink(x)
        true
    end, arr)
end

# ===== Complex Example =====

function getHistory(name, owner)
    result = source()
    return result
end

function test_original_example()
    history = getHistory("name", "owner")
    customForEach([history], node -> begin
        changes = node
        # ruleid: test-hof-taint
        sink(changes)
    end)
end

# Stub functions
function source()
    return "tainted"
end

function sink(s)
end

# ===== Top-level HOF Tests =====
# These test HOF callback detection at module level (outside any function)

# Top-level lambda callback
# ruleid: test-hof-taint
toplevel_sink = x -> sink(x)
toplevel_sink(source())

# Top-level function HOF (map with named callback)
function toplevel_handler(x)
    # ruleid: test-hof-taint
    sink(x)
    return x
end

toplevel_items = [source()]
map(toplevel_handler, toplevel_items)

# Top-level user-defined HOF
customForEach(toplevel_items, toplevel_handler)
