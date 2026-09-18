local function sink(x)
  print(x)
end

function emit(data)
  -- ok: test-sanitiser
  sink(data)
end
