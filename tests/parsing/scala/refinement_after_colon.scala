// A refinement can follow a colon at the end of the line, with the
// declarations indented.
object A:
  type Aux[A, T] =
    Fields[A]:
      type AsTuple = T
      def size: Int
  type B = C
