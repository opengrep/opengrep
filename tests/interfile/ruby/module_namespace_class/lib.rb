# Nested `module Svc; class Base` — the classic namespace form.  This
# used to work only because a Ruby-specific reshape rewrote the module
# into a class.  With the reshape retired, `module` is handled directly
# as a namespace scope; this test guards that `Base` still gets the qn
# `Svc.Base` (identical to the compact `class Svc::Base` form) so
# `class Client < Svc::Base` in app.rb resolves and inherits `handle`.
module Svc
  class Base
    def handle(x)
      # ruleid: module-namespace-class
      sink(x)
    end
  end
end
