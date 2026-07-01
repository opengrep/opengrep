void f(obj) {
  // a cascade-syntax pattern binds the real receiver via the desugared binder
  // MATCH:
  obj..save();
}
