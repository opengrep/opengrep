<?hh
function f(): void {
  //MATCH:
  list($a) = $arr;

  //MATCH: a trailing comma is not an element
  list($a, ) = $arr;

  //OK: two elements
  list(, $b) = $arr;
}
