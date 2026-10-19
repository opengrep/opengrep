object InlineUsingParameters:
  inline def apply[A](using inline z: Show[A]): Show[A] = z
  inline given canFail[E](using inline ev: NotGiven[E =:= Nothing]): CanFail[E] = CanFail
  def types(using Show[Int], Ordering[Int]) = 1
