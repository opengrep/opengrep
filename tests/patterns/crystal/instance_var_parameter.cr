class A
  # ERROR:
  def initialize(@x : String)
  end

  def initialize(@x : String)
    puts(@x)
  end
end
