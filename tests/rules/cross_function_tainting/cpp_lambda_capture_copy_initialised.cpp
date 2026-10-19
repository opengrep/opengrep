// The initialiser is evaluated where the lambda is created.
void copies_the_tainted_value() {
  int a = source();
  auto f = [y = a]() {
    // ruleid: cpp_lambda_capture_copy_initialised
    sink(y);
  };
  f();
}

// A copy does not see a later assignment.
void copies_before_the_taint() {
  int a = 0;
  auto f = [y = a]() {
    // ok: cpp_lambda_capture_copy_initialised
    sink(y);
  };
  a = source();
  f();
}

// The initialised capture is the lambda's own variable.
void shadows_the_tainted_variable() {
  int x = source();
  auto f = [x = 0]() {
    // ok: cpp_lambda_capture_copy_initialised
    sink(x);
  };
  f();
}

// A source in the initialiser taints the new variable.
void initialises_with_the_source() {
  auto f = [y = source()]() {
    // ruleid: cpp_lambda_capture_copy_initialised
    sink(y);
  };
  f();
}
