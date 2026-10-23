object PolymorphicFunctionLiterals:
  val headOption = [A] => (xs: List[A]) => xs.headOption
  val bounded: [U <: AnyRef] => U => Int = [U <: AnyRef] => (x: U) => x.hashCode
  val lifted = FunctionK.lift[List, Option]([X] => (_: List[X]).headOption)
  def block = transform {
    [A] =>
      (fa: F[A]) => fa
  }
