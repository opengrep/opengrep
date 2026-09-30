require_relative 'child'

def run
  Child.new.handle(source())
end

def run_own
  OwnChild.new.handle(source())
end
