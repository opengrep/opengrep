void f() {
  var a = (x) => x;
  // MATCH:
  var b = (x) async => x;
}
