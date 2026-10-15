// a leading '::' only says that a name is looked up from the global
// namespace, so a qualified pattern without one matches the name with or
// without it

void f() {
  // MATCH:
  ns::foo(3);

  // MATCH:
  ::ns::foo(4);

  // OK: a different namespace
  ::other::foo(5);

  // OK: unqualified
  foo(1);
}
