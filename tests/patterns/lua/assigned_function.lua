function M.f() end
function f() end
-- ERROR: match
M.g = function() end
local function h() end
function M:k() end
