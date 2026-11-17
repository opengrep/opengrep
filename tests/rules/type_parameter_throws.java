class E {}

class C {
  // ruleid: throws-type-parameter
  <E extends Exception> void m() throws E {
  }

  // ok: throws-type-parameter
  <E extends Exception> void k() throws Exception {
  }
}
