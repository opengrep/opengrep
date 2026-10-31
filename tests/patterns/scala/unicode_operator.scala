// An operator can start with a Unicode symbol and go on with ASCII operator
// characters, as `∈:`.
object O {
  //ERROR:
  def ∈:(a: Int): Boolean = true
  //OK:
  def ∈(a: Int): Boolean = true
}
