void set(std::string &x) { x = source(); }

void caller() {
  std::string s = "";
  set(s);
  // ruleid: test-param-by-reference-cpp
  sink(s);
}
