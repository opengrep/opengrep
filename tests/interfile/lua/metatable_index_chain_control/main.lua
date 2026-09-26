local Base = {}
Base.__index = Base

function Base:handle(x)
  -- ok: metatable-index-chain-control
  sink(x)
end

local Other = {}

Other:handle(source())
