void f(int a) {
  //ERROR: match
  auto l1 = [y(a)](){ use(y); };
  // OK:
  auto l2 = [y{a}](){ use(y); };
  // OK:
  auto l3 = [y = {a}](){ use(y); };
  // OK:
  auto l4 = [y = a](){ use(y); };
}
