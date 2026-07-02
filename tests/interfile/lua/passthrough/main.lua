require("mid")

local function source()
  return os.getenv("SECRET")
end

local t = source()
relay(t)
