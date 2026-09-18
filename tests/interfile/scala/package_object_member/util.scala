package a

package object util {
  def sink(x: String): Unit = println(x)

  def handle(msg: String): Unit = {
    // ruleid: test-package-object-member
    sink(msg)
  }
}
