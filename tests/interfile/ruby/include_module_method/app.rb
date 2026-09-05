require_relative 'helpers'

class App
  # `include Helpers` is parsed as a regular method call in the class
  # body, not as a Ruby parent in the class entity.  projidx must
  # detect it via class_body_extra_parents and add Helpers to App's
  # MRO so that `process` resolves to Helpers#process across files.
  include Helpers
end

def source
  ENV["INPUT"]
end

def main
  tainted = source
  App.new.process(tainted)
end

main
