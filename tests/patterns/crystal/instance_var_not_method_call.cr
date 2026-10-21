class A
  def reads
    # ERROR:
    puts(@x)
    puts(self.x)
  end
end
