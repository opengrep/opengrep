const tainted = source();

class K {
  static tainted = "safe";

  m() {
    // ruleid: test-static-member-scope-js
    sink(tainted);
  }

  static s() {
    // ok: test-static-member-scope-js
    sink(this.tainted);
  }
}
