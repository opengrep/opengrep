<?hh
function f(): void {
  //OK: a skipped element is not an expression
  list(, $b) = $arr;

  //MATCH:
  list($a, $b) = $arr;

  //OK: three elements
  list($a, , $c) = $arr;
}
