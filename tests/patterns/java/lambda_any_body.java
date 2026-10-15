class A {
  void m() {
    // A lambda pattern with [...] as its body matches every body.
    //ERROR: match
    run(x -> foo(x));
    //ERROR: match
    run(x -> { return foo(x); });
    //ERROR: match
    run(x -> { foo(x); });
    //ERROR: match
    run(x -> { a(); b(); });
  }
}
