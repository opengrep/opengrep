local function sink(x)
  print(x)
end

function other(msg)
  -- ok: test-global-table
  sink(msg)
end
