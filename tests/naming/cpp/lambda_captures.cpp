int f(int a) {
  int b = a;
  int y = 0;
  auto l = [a, &b, y = b + 1]() { return a + b + y; };
  return y;
}
