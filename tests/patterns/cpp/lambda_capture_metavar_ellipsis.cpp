void f(int a, int b) {
  //ERROR: match
  auto l1 = [&a](){ use(a); };
  //ERROR: match
  auto l2 = [b, &a](){ use(a, b); };
  //ERROR: match
  auto l3 = [=, &a](){ use(a, b); };
  auto l4 = [a](){ use(a); };
  auto l5 = [](){ use(1); };
}
