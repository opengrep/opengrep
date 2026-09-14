local function sink(x)
  print(x)
end

function handle(msg)
  -- ruleid: test-global-table
  sink(msg)
end
