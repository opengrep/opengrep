object NamedTuples:
  type Person = (name: String, age: Int)
  val bob = (name = "Bob", age = 33)
  def closed: Option[(byMod: Boolean, forever: Boolean)] = None
  def unapplyName(p: Person) = p match
    case (name = n, age = a) => n
  def city(c: City) = c match
    case City(population = p, name = n) => p
  val (state = game, moves = moves) = play()
  def dependent: (e: Entry) => e.Key = ???
  def assign(x: Int) =
    var y = 0
    (y = x)

object BackquotedNamedPattern:
  def f(p: Any) = p match
    case (`type` = t) => t
