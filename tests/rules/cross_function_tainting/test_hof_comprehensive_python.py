# Comprehensive HOF test for Python: Custom higher-order functions
# All of these should detect taint flow from source() to sink()

# ===== Custom HOF Functions =====

# Manual loop implementation
def custom_map_loop(arr, callback):
    result = []
    for item in arr:
        result.append(callback(item))
    return result

# Delegates to built-in (tests ToSinkInCall propagation)
def custom_map_builtin(arr, callback):
    return list(map(callback, arr))

def custom_for_each(arr, callback):
    for item in arr:
        callback(item)

def direct_call(callback):
    callback(source())

# ===== Test Cases =====

# Test custom HOF with manual loop + lambda
def test_custom_map_loop_lambda():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_map_loop(arr, lambda x: (
        sink(x)
    ))

def process_custom_map_loop(x):
    # ruleid: test-hof-taint
    sink(x)
    return x

# Test custom HOF with manual loop + named function
def test_custom_map_loop_named():
    arr = [source()]
    custom_map_loop(arr, process_custom_map_loop)

# Test custom HOF delegating to built-in + lambda
def test_custom_map_builtin_lambda():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_map_builtin(arr, lambda x: (
        sink(x)
    ))

def process_custom_map_builtin(x):
    # ruleid: test-hof-taint
    sink(x)
    return x

# Test custom HOF delegating to built-in + named function
def test_custom_map_builtin_named():
    arr = [source()]
    custom_map_builtin(arr, process_custom_map_builtin)

def test_custom_for_each():
    arr = [source()]
    # ruleid: test-hof-taint
    custom_for_each(arr, lambda x: (
        sink(x)
    ))

def test_direct_call():
    # ruleid: test-hof-taint
    direct_call(lambda x: (
        sink(x)
    ))

# ===== Built-in methods (if supported) =====

def sink_and_return(x):
    # ruleid: test-hof-taint
    sink(x)
    return x

def test_builtin_map():
    arr = [source()]
    list(map(sink_and_return, arr))

def sink_and_return_true(x):
    # ruleid: test-hof-taint
    sink(x)
    return True

def test_builtin_filter():
    arr = [source()]
    list(filter(sink_and_return_true, arr))

# ===== Complex Example =====

def get_history(name, owner):
    result = source()
    return result

def test_original_example():
    history = get_history("name", "owner")
    items = []
    for node in history:
        changes = node.associated_pull_requests.nodes
        # ruleid: test-hof-taint
        sink(changes)

# ===== Top-level HOF Tests =====
# These test HOF callback detection at module level (outside any function)

# Top-level lambda callback
# ruleid: test-hof-taint
toplevel_sink = lambda x: sink(x)
toplevel_sink(source())

# Top-level function HOF (map with named callback)
def toplevel_handler(x):
    # ruleid: test-hof-taint
    sink(x)
    return x

toplevel_items = [source()]
list(map(toplevel_handler, toplevel_items))

# Top-level user-defined HOF
custom_for_each(toplevel_items, toplevel_handler)
