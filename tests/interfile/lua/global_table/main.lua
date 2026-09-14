local function source()
  return os.getenv("SECRET")
end

local t = source()
handle(t)
