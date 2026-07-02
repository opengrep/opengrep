require("impl")

local function source()
  return os.getenv("SECRET")
end

local tainted = source()
greet(tainted)
