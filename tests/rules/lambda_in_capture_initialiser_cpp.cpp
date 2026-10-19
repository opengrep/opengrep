// The initialiser of a capture of an assigned lambda.
void assigned() {
  // ruleid: lambda_in_capture_initialiser_cpp
  auto f = [g = []() { int a = source(); sink(a); }]() { g(); };
  f();
}

// The initialiser of a capture of a lambda passed as an argument.
void passed() {
  // ruleid: lambda_in_capture_initialiser_cpp
  run([g = []() { int a = source(); sink(a); }]() { g(); });
}

// An untainted value in the lambda of a capture initialiser.
void untainted() {
  // ok: lambda_in_capture_initialiser_cpp
  auto f = [g = []() { int a = 0; sink(a); }]() { g(); };
  f();
}
