class C {
 public:
  std::string x;
};

void byValue(C c) { c.x = source(); }
void byRef(C &c) { c.x = source(); }

void caller() {
  C a;
  byValue(a);
  // ok: test-value-copies-cpp
  sink(a.x);
  C b;
  byRef(b);
  // ruleid: test-value-copies-cpp
  sink(b.x);
}
