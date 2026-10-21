class Foo {
public:
  Foo(int x);
  Foo &assign(const Foo &other);
  int x;
};

Foo make() { return Foo(1); }
