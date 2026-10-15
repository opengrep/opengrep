void f(int a, int b) {
  //ERROR: match
  auto l1 = [&a](){ use(a); };
  //ERROR: match
  auto l2 = [&a, &b](){ use(a, b); };
  auto l3 = [&a, b](){ use(a, b); };
  auto l4 = [a](){ use(a); };
}
