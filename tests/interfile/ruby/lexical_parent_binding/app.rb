require_relative 'o'
require_relative 'deep/svc_base'

# `Box` is defined inside `module Svc`, so `< Base` resolves lexically to
# `Svc::Base` — NOT the top-level-ish `Other::Base`.  Two `Base` classes share
# the leaf name, so this only resolves correctly when parent scoring uses the
# constant path (`Svc.Box` prefers `Svc.Base`).
module Svc
  class Box < Base
  end
end

def source
  ENV["X"]
end

def main
  t = source
  Svc::Box.new.handle(t)
end

main
