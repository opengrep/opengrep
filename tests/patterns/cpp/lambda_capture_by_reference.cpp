void f(int a, int b) {
  //ERROR: match
  auto l1 = [&a](){ use(a); };
  // OK:
  auto l2 = [b, &a](){ use(a); };
  // OK:
  auto l3 = [=, &a](){ use(a); };
  // OK:
  auto l4 = [a](){ use(a); };
  // OK:
  auto l5 = [&](){ use(a); };
}
