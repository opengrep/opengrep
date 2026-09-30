require_relative 'child'

def run
  Child.new.handle(source())
end

def run_audited
  Audited.new.audit(source())
end

def run_own
  OwnMember.new.handle(source())
end
