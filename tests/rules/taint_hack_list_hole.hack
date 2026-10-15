<?hh
// a skipped element keeps the later elements at their index

function first_hole(): void {
  list(, $b) = vec[source(), "safe"];
  // ok: taint-hack-list-hole
  sink($b);
}

function first_hole_tainted(): void {
  list(, $b) = vec["safe", source()];
  // ruleid: taint-hack-list-hole
  sink($b);
}

function trailing_comma(): void {
  list($a, ) = vec[source(), "safe"];
  // ruleid: taint-hack-list-hole
  sink($a);
}
