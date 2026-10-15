struct S {
  int m;
  void f(int a) {
    //ERROR: match
    auto l1 = [=, &a](){ use(a); };
    //ERROR: match
    auto l2 = [=, this](){ use(m); };
    // OK:
    auto l3 = [a](){ use(a); };
    // OK:
    auto l4 = [&, a](){ use(a); };
  }
};
