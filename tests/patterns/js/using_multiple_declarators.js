function f() {
  // MATCH:
  using a = openA(), b = openB();
  const c = openB();
}
