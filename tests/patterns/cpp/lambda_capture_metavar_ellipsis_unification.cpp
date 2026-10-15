void f(int a, int b) {
  //ERROR: match
  auto l1 = [a, &b](){ use(a); };
  auto l2 = [a, &b](){ use(b); };
}

void g(int a, int b) {
  auto l1 = [a, &b](){ use(a); };
  auto l2 = [a](){ use(b); };
}
