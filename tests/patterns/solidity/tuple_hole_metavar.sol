contract C {
  function f() public {
    //OK: a skipped component is not an expression
    (, b) = g();

    //MATCH:
    (a, b) = g();

    //OK: three components
    (a, , c) = g();
  }
}
