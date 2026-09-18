local function sink(x)
  print(x)
end

function leak(x)
  -- ruleid: test-passthrough
  sink(x)
end
