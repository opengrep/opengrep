require_relative 'lib'

# `Svc::Base` is defined in compact form in lib.rb.  For this parent
# reference to resolve across files, lib.rb's class must carry the qn
# `Svc.Base` (via qualifier decomposition), matching the path `Svc.Base`
# extracted from `< Svc::Base` here.
class Client < Svc::Base
end

def source
  ENV["INPUT"]
end

def main
  tainted = source
  Client.new.handle(tainted)
end

main
