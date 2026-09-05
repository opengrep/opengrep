require_relative 'service'

def vulnerable
  tainted = source()
  result = CreateService.new("user", tainted).execute("group")
  # ruleid: test-service-passthrough
  sink(result)
end
