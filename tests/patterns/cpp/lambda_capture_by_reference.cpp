void f(int x) {
  // ERROR:
  auto a = [&x]() { use(x); };
  auto b = [x]() { use(x); };
  auto c = []() { use(1); };
  auto d = [&]() { use(x); };
}
