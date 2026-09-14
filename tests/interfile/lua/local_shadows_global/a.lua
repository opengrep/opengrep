local function sink(x)
  print(x)
end

local function handle(msg)
  -- ok: test-local-shadows-global
  sink(msg)
end

handle("clean")
