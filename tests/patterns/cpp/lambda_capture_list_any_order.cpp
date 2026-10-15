void f(int a, int b, int c) {
  //ERROR: match
  auto l1 = [a, &b](){ use(a, b); };
  //ERROR: match
  auto l2 = [&b, a](){ use(a, b); };
  // OK:
  auto l3 = [&a, b](){ use(a, b); };
  // OK:
  auto l4 = [a, &b, c](){ use(a, b); };
}
