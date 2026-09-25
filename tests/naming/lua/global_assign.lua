local function early()
  return later()
end

f = function () return 1 end
M = {}

function M.g()
  return f()
end

local function h()
  count = 1
  local n = 1
  n = 2
  return n
end

later = function () return 2 end

h()
print(count, M, early())
