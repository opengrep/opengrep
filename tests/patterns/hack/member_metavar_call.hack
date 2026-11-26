function f($o, $m) {
  // ERROR:
  $o->run();
  // ERROR:
  $o->$m();
  run();
}
