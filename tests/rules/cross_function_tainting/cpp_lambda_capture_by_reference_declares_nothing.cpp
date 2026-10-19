// The capture [&w] refers to the condition variable [w]: it declares no
// variable where the lambda is written.
void condition_variable_captured_by_reference() {
  if (int w = source()) {
    auto l = [&w]() { g(w); };
    // ruleid: cpp_lambda_capture_by_reference_declares_nothing
    sink(w);
  }
}

void untainted_condition_variable() {
  if (int w = 0) {
    auto l = [&w]() { g(w); };
    // ok: cpp_lambda_capture_by_reference_declares_nothing
    sink(w);
  }
}
