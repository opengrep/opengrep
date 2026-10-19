void f(int a) {
  //ERROR: match
  auto l1 = [](){ use(1); };
  //ERROR: match
  auto l2 = [a](){ use(a); };
  //ERROR: match
  auto l3 = [&](){ use(a); };
  //ERROR: match
  auto l4 = [=, &a](){ use(a); };
}
