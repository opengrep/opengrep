function f(): void {
  $a = ($x) ==> $x;
  // MATCH:
  $b = async ($x) ==> $x;
}
