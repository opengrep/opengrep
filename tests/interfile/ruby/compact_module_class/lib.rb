# Compact `class Svc::Base` — the `Svc::` qualifier lives INSIDE the
# class entity name (an `IdQualified`), not in a surrounding `module`.
# projidx must decompose it into a namespace scope so this class's qn is
# `Svc.Base` and unifies with the nested `module Svc; class Base` form.
# Without the decomposition the class looks top-level (`Base`), and
# `class Client < Svc::Base` in app.rb resolves its parent by the path
# `Svc.Base`, which never matches the bare `Base` — so `handle` is never
# inherited and the taint never reaches this sink.
class Svc::Base
  def handle(x)
    # ruleid: compact-module-class
    sink(x)
  end
end
