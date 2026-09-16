-- ERROR: match
function M.f() end
-- ERROR: match
function f() end
M.g = function() end
-- ERROR: match
local function h() end
function M:k() end
