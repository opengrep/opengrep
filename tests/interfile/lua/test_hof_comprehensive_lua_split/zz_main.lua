-- Comprehensive HOF test for Lua: Custom higher-order functions
-- All of these should detect taint flow from source() to sink()

-- ===== Custom HOF Functions =====
-- Lua doesn't have built-in HOF functions, so we test manual loop implementations




-- ===== Test Cases =====




-- ===== Complex Example =====



-- Stub functions


-- ===== Top-level HOF Tests =====
-- These test HOF callback detection at script level (outside any function)

-- Top-level lambda callback
-- ruleid: test-hof-taint
local toplevel_sink = function(x) sink(x) end
toplevel_sink(source())

-- Top-level named callback function

-- Top-level user-defined HOF with named callback
local toplevel_items = {source()}
customMap(toplevel_items, toplevel_handler)

-- Top-level user-defined HOF
customForEach(toplevel_items, toplevel_handler)
