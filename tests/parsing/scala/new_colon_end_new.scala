// After `new A:`, a colon at the end of a line before `end new` starts an
// empty body.
trait Foo
val foo =
  new Foo:
    // comment
  end new
