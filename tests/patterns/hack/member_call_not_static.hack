function f($db): void {
  // ERROR:
  $db->bar();
  Foo::bar();
}
