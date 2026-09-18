def send_clean(value)
  safe = sanitize(value)
  # ok: sanitiser-active-ruby
  sink(safe)
end

def send_dirty(value)
  # ruleid: sanitiser-active-ruby
  sink(value)
end
