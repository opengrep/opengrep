class Fp
  def initialize(p)
    @a = source()
  end

  def reads_missing
    # ok: ruby_ctor_no_leak
    sink(@zzz)
  end

  def no_field
    x = "safe"
    # ok: ruby_ctor_no_leak
    sink(x)
  end

  def literal
    # ok: ruby_ctor_no_leak
    sink("safe")
  end
end
