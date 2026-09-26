local Base = {}
Base.__index = Base

function Base:handle(x)
  -- ruleid: metatable-index-chain
  sink(x)
end

local Derived = setmetatable({}, {__index = Base})
Derived.__index = Derived

Derived:handle(source())
