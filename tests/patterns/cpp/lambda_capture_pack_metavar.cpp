template <typename... T>
void f(T... xs) {
  //ERROR: match
  auto l1 = [&xs...](){ use(xs...); };
  // OK:
  auto l2 = [xs...](){ use(xs...); };
}
