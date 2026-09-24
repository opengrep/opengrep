function* gen() {
  const x = yield source();
  // ok: yield_sent_value
  sink(x);
}
