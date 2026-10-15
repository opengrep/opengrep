contract C {
  function f() public {
    //MATCH:
    (, b) = g();

    //MATCH:
    (a, b) = g();

    //OK:
    (, c) = g();
  }
}
