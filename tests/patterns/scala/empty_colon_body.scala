// A colon at the end of a line before no indented line starts an empty body.
object A:
  object client:
    // reserved for later
  end client
  //ERROR:
  def f = sink(source())

trait B:
end B

class C extends D:
  // nothing yet
