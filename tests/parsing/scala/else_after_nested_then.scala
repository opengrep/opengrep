// An `else` at the column of an `if` in a case body belongs to that `if`,
// not to the `if` nested in its `then` branch.
object A:
  def f(tree: Tree) =
    tree match
      case t =>
        if a then
          if b then
            c
        else if (d)
          && e
        then
          g()
