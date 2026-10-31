// `end` after a dot is a member, not an end marker.
object A:
  def g =
    f.end match
      case 1 => 2
