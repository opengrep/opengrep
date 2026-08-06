// Structured binding. The C++ parser wraps the pattern in a PatTyped,
// so is_destructuring_pattern has to look through it.
void decl_form() {
  auto [a, b] = taint_source();

  // ruleid: taint-cpp-destructuring
  sink(a);

  // ruleid: taint-cpp-destructuring
  sink(b);
}

// A single (non-destructuring) declaration keeps its existing lowering.
void single_binding() {
  auto c = taint_source();

  // ruleid: taint-cpp-destructuring
  sink(c);
}

void clean() {
  auto d = 1;

  // ok: taint-cpp-destructuring
  sink(d);
}
