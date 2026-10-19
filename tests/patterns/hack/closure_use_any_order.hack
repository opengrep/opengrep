function f(): void {
  //ERROR:
  $a = function() use ($x, $y) { g($x, $y); };
  //ERROR:
  $b = function() use ($y, $x) { g($x, $y); };
  // OK:
  $c = function() use ($x) { g($x); };
}
