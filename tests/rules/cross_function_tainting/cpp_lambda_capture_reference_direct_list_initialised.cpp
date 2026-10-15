// A reference direct-list-initialised with a variable is that variable.
void aliases_the_variable_direct_list() {
  int a = source();
  auto f = [&r{a}]() {
    // ruleid: cpp_lambda_capture_reference_direct_list_initialised
    sink(r);
  };
  f();
}
