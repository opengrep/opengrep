def run(enabled)
  # ERROR:
  if enabled
    audit("start")
    dangerous(user_input)
    audit("done")
  end
end
