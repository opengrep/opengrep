function f(): void {
  //ERROR:
  $a = function() use ($x) { g($x); };
  // OK:
  $b = function() use ($y, $x) { g($x); };
  // OK:
  $c = function() use ($y) { g($x); };
  // OK:
  $d = function() { g($x); };
}
