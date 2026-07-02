require("mid")
require("impl")

local function source()
  return os.getenv("SECRET")
end

local x = source()
local y = process(x)
emit(y)
