# A homonym `Base` in a DIFFERENT namespace (`Other`) at a SHORT file path.
# With filename-prefixed class qns, this shorter qn used to win parent
# resolution for `Svc::Box < Base` in app.rb — binding to the wrong (benign)
# class purely by filename length.  With constant-path Ruby identity, `Box`
# (qn `Svc.Box`) shares `Svc` with `Svc::Base`, not with `Other::Base`, so
# lexical scope decides.
module Other
  class Base
    def handle(x)
      # ok: lexical-parent-binding
      benign(x)
    end
  end
end
