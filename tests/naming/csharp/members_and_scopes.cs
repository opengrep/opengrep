namespace N {
  class A {
    int fa = 1, fb = 2;
    int P { get; set; }
    event H E { add {} remove {} }
    void M(int x, object o) {
      int a = 1, b = 2;
      foo(a, b, fa, fb, P, E);
      using (var r = o, s = o) { foo(r, s); }
      switch (x) {
        case 1:
          int y = 1;
          foo(y);
          break;
        case 2:
          y = 2;
          foo(y);
          break;
      }
    }
  }
  class B { void K() { A q = null; } }
}
