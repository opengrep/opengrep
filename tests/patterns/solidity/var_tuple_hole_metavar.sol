contract C {
  function f() public {
    //OK: a skipped component is not an expression
    var (x, , y) = g();

    //MATCH:
    var (x, z, y) = g();
  }
}
