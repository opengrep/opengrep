# The lexically-correct parent: `Svc::Base`, at a LONGER file path.  `Svc::Box`
# in app.rb must bind HERE (shared `Svc` scope), reaching this sink.
module Svc
  class Base
    def handle(x)
      # ruleid: lexical-parent-binding
      sink(x)
    end
  end
end
