@JsonSerialize(using = classOf[DataTypeSerializer])
class UsingIdentifier {
  def param(using: Int) = using + 1
  def call = param(using = 1)
  def clause(x: Int)(using ctx: String) = ctx
  def args = clause(1)(using "a")
}
