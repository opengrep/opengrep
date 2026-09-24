class Store
  attr_reader :name

  def initialize(name)
    @name = source()
    @constant = "safe"
  end

  def by_reader
    # ruleid: ruby_ivar_across_methods
    sink("SELECT * FROM #{name}")
  end

  def by_ivar
    # ruleid: ruby_ivar_across_methods
    sink("SELECT * FROM #{@name}")
  end

  def by_other_ivar
    # ok: ruby_ivar_across_methods
    sink(@constant)
  end

  def same_method(x)
    @n = source()
    # ruleid: ruby_ivar_across_methods
    sink("SELECT * FROM #{@n}")
  end
end
