function f() {
  // MATCH:
  using file: File = open();
  const other: File = open();
}
