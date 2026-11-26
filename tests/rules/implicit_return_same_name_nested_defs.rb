def outer(c)
  if c
    def inner
      # ruleid: implicit-return-same-name-nested-defs
      foo(1)
    end
  else
    def inner
      # ruleid: implicit-return-same-name-nested-defs
      foo(2)
    end
  end
  # ok: implicit-return-same-name-nested-defs
  foo(3)
  bar(0)
end
