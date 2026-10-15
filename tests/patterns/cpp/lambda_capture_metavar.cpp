void f(int a, int b) {
  //ERROR: match
  auto l1 = [&a](){ use(a); };
  //ERROR: match
  auto l2 = [&b](){ use(b); };
  // OK:
  auto l3 = [a](){ use(a); };
  // OK:
  auto l4 = [&a, &b](){ use(a); };
  // OK:
  auto l5 = [&](){ use(a); };
}
