package m

import a.{Base => _, _}
import b._

class Sub extends Base {
  def run(): Unit = f(source())
}
