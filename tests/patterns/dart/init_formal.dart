class Foo {
  int x;
  int y;

  // An initializing formal `this.x` has no `this.x = x` in the source, so
  // the pattern must NOT match here (no ERROR annotation).
  Foo(this.x);

  void setY(int v) {
    // ERROR:
    this.y = v;
  }
}
