local Base = {}

function Base:handle(x)
  -- ruleid: metatable-index-bound-table
  sink(x)
end

local mt = {}
mt.__index = Base

Derived = {}
setmetatable(Derived, mt)

Derived:handle(source())
