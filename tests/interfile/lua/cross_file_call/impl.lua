local function sink(x)
  print(x)
end

function greet(msg)
  -- ruleid: test-cross-file-call
  sink(msg)
end
