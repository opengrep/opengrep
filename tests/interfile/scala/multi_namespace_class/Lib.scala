// Name collision across two braced package regions.  `a.Base.handle` is
// benign; `b.Base.handle` sinks.  `class Client extends Base` (imported from
// `b`) must inherit the sink via the per-region qn + type-driven MRO dispatch.
package a {
  class Base {
    def handle(x: String): Unit = {
      // ok: multi-namespace-class
      ()
    }
  }
}

package b {
  class Base {
    def handle(x: String): Unit = {
      // ruleid: multi-namespace-class
      sink(x)
    }
  }
}
