// A reference direct-initialised with a variable is that variable.
void aliases_the_variable_direct() {
  int a = source();
  auto f = [&r(a)]() {
    // ruleid: cpp_lambda_capture_reference_direct_initialised
    sink(r);
  };
  f();
}
