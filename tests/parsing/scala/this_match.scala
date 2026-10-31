class C:
  def f = this.match
    case _: C => 1
  def g = (this.match { case _ => 2 })
