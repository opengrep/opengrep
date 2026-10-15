// a leading '::' only says that a name is looked up from the global
// namespace, so a pattern without one matches the name with or without it

void f() {
  // MATCH:
  foo(1);

  // MATCH:
  ::foo(2);

  // OK: a function of a namespace
  ns::foo(3);

  // OK: the same, from the global namespace
  ::ns::foo(4);
}
