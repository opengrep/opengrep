def run
  # ERROR:
  log("auth", user.id, request.ip)
  audit
end
