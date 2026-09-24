void by_value() {
  int x = 0;
  // ok: test-capture-modes-cpp
  auto l = [x]() { sink(x); };
  x = source();
  l();
}

void by_reference() {
  int x = 0;
  // ruleid: test-capture-modes-cpp
  auto l = [&x]() { sink(x); };
  x = source();
  l();
}

void default_by_value() {
  int x = 0;
  // ok: test-capture-modes-cpp
  auto l = [=]() { sink(x); };
  x = source();
  l();
}

void default_by_reference() {
  int x = 0;
  // ruleid: test-capture-modes-cpp
  auto l = [&]() { sink(x); };
  x = source();
  l();
}

void default_by_value_with_reference() {
  int x = 0;
  int y = 0;
  auto l = [=, &y]() {
    // ok: test-capture-modes-cpp
    sink(x);
    // ruleid: test-capture-modes-cpp
    sink(y);
  };
  x = source();
  y = source();
  l();
}

void initialised() {
  // ruleid: test-capture-modes-cpp
  auto l = [y = source()]() { sink(y); };
  l();
}
