local function sink(x)
  print(x)
end

local function source()
  return os.getenv("SECRET")
end

local function handle(msg)
  -- ok: test-local-shadows-global
  sink(msg)
end

local function report(msg)
  -- ruleid: test-local-shadows-global
  sink(msg)
end

handle("clean")
report(source())
