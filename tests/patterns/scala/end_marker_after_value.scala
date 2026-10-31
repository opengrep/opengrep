// The end marker of the value of a definition is no statement of its body.
object A:
  //ERROR:
  def f(x: Int) =
    x match
      case 1 => 2
      case _ => 3
    end match
