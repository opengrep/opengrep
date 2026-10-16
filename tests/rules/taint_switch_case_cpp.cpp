void test(int mode) {
  const int CMD = 7;
  int q = 0;
  // ruleid: switch-case-constant-propagation-cpp
  exec(CMD);
  switch (mode) {
    case CMD:
      // ruleid: switch-case-constant-propagation-cpp
      exec(CMD);
      break;
    case B:
      q = taint_source();
      // ruleid: taint-switch-case-cpp
      sink(q);
      break;
  }
  // ruleid: switch-case-constant-propagation-cpp
  exec(CMD);
}

// Qualified and bare case labels read values; neither may inherit the
// scrutinee's taint through a synthetic pattern binding.
void caseLabelIsNotABindingTarget() {
  int y = taint_source();
  switch (y) {
    case Foo::BAR:
      break;
    default:
      break;
  }

  // ok: taint-switch-case-cpp
  sink(Foo::BAR);

  // Taint out of the scrutinee itself is unaffected.
  // ruleid: taint-switch-case-cpp
  sink(y);
}

void bareNameCaseIsNotABindingTarget() {
  int y = taint_source();
  switch (y) {
    case RED:
      break;
  }
  // ok: taint-switch-case-cpp
  sink(RED);
}

void castLabelsReadValues() {
  int mode = taint_source();
  int value = 0;
  switch (mode) {
    case (int)FIRST:
      value = 0;
      break;
    case (int)SECOND:
      value = taint_source();
      // ruleid: taint-switch-case-cpp
      sink(value);
      break;
  }
  // ok: taint-switch-case-cpp
  sink(FIRST);
}
