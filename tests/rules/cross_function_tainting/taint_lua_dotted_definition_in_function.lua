local M = {}
function g()
  function M.f(x)
    -- ruleid: taint_lua_dotted_definition_in_function
    sink(x)
  end
  M.f(source())
end
g()
