class A {
  void m() {
    // An expression body is the lambda's value.
    //ERROR: match
    run(x -> foo(x));
    //ERROR: match
    run(x -> { return foo(x); });
    run(x -> { foo(x); });
  }
}
