// `end given` is an end marker.
object Wr:
  given [A](using ra: Render[A]): Render[Wr[A]] with
    def asString(value: Wr[A]): String = ra.asString(value)
  end given
end Wr
