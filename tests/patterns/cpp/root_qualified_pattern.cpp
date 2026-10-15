// a pattern with a leading '::' asks for the name in the global namespace,
// so it matches only the name written with one

void f() {
  // MATCH:
  ::foo(2);

  // OK: no leading '::'
  foo(1);
}
