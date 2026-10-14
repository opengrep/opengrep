<?hh
function f(): void {
  //OK: a skipped element is not an expression
  foreach ($rows as list(, $v)) {}

  //MATCH:
  foreach ($rows as list($k, $v)) {}
}
