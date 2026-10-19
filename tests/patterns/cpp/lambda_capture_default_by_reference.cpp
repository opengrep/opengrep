void f(int a) {
  //ERROR: match
  auto l1 = [&](){ use(a); };
  // OK:
  auto l2 = [=](){ use(a); };
  // OK:
  auto l3 = [&a](){ use(a); };
  // OK:
  auto l4 = [&, a](){ use(a); };
}
