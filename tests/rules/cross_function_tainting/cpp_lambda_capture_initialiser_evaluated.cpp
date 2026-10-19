// The initialiser is evaluated even if the lambda is never called.
void evaluates_the_initialiser() {
  int x = source();
  // ruleid: cpp_lambda_capture_initialiser_evaluated
  auto f = [y = sink(x)]() {};
}
