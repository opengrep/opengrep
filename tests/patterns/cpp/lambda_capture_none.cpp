void f(int a) {
  //ERROR: match
  auto l1 = [](){ use(1); };
  // OK:
  auto l2 = [a](){ use(a); };
  // OK:
  auto l3 = [&](){ use(a); };
  // OK:
  auto l4 = [=](){ use(a); };
}
