async function f() {
  // MATCH:
  await using conn = connect();
  using other = connect();
}
