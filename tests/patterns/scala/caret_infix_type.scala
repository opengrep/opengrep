// `^` after a space is an infix type operator, not a capture annotation.
object T {
  //ERROR:
  type Y = A ^
    B
  //ERROR:
  type W = A ^ B
  // `A^` before a line indented further continues the type
  //ERROR:
  type V = A^
    B
  //OK:
  type Z = A^
  //OK:
  type U = A^
}
