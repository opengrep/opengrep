void main(a, baz) {
  // a semgrep ellipsis can surround a named argument in a call
  // MATCH:
  var w = foo(a, bar: baz, quux: 2);
}
