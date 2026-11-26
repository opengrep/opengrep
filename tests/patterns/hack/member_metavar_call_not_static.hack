function f($o, $cls): void {
  // ERROR:
  $o->run();
  Foo::run();
  $cls::run();
}
