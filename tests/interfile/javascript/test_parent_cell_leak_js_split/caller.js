function caller() {
  const r = helper();
  const { b } = r;
  sink(b);
}
