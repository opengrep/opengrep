void f(int x, int y) {
  // ERROR:
  auto a = [&x, y]() { use(x); };
  // ERROR:
  auto b = [y, &x]() { use(x); };
  // ERROR:
  auto c = [&x]() { use(x); };
  // ERROR:
  auto d = [=, &x]() { use(x); };
  auto e = [x, y]() { use(x); };
  auto g = [y]() { use(y); };
}
