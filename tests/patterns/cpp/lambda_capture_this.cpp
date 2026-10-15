struct S {
  int v;
  void f() {
    //ERROR: match
    auto l1 = [this](){ use(v); };
    // OK:
    auto l2 = [=, this](){ use(v); };
    // OK:
    auto l3 = [*this](){ use(v); };
    // OK:
    auto l4 = [=](){ use(v); };
  }
};
