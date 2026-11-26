function f($o): void {
  // ERROR:
  $o->p = 1;
  Foo::$p = 1;
}
