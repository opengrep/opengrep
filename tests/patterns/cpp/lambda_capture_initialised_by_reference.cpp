void f(int a) {
  //ERROR: match
  auto l1 = [&r = a](){ use(r); };
  // OK:
  auto l2 = [&r(a)](){ use(r); };
  // OK:
  auto l3 = [&r{a}](){ use(r); };
  // OK:
  auto l4 = [r = a](){ use(r); };
}
