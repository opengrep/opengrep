void f(int a) {
  //ERROR: match
  auto l1 = [x = a](){ use(x); };
  // OK:
  auto l2 = [x(a)](){ use(x); };
  // OK:
  auto l3 = [x{a}](){ use(x); };
  // OK:
  auto l4 = [&x = a](){ use(x); };
  // OK:
  auto l5 = [x](){ use(x); };
}
