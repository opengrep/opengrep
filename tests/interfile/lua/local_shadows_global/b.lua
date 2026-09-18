local function sink(x)
  print(x)
end

function handle(msg)
  -- ruleid: test-local-shadows-global
  sink(msg)
end

function report(msg)
  -- ok: test-local-shadows-global
  sink(msg)
end
