-- The value of g is f, a member access on a call of g itself. A function
-- assigns g too, so a call of g inside a function reaches every value of g,
-- h among them.
local function h()
  return source()
end

local function clean()
  return 0
end

local g = h
local f = g().m
g = f

local function reset()
  g = h
end

local function run()
  -- ruleid: resolution_cycle_through_call_lua
  sink(g())
  -- ok: resolution_cycle_through_call_lua
  sink(clean())
end
