class A
  # ERROR:
  def m(@x : String)
  end

  # ERROR:
  def n(x : String)
  end

  def k(x : Int32)
  end
end
