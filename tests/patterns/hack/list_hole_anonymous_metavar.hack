<?hh
function f(): void {
  //MATCH:
  list(, $b) = $arr;

  //MATCH:
  list($a, $b) = $arr;

  //OK:
  list(, $c) = $arr;
}
