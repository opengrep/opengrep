void test(int mode) {
  const int CMD = 7;
  int q = 0;
  // ruleid: switch-case-constant-propagation-c
  exec(CMD);
  switch (mode) {
    case CMD:
      // ruleid: switch-case-constant-propagation-c
      exec(CMD);
      break;
    case B:
      q = taint_source();
      // ruleid: taint-switch-case-c
      sink(q);
      break;
  }
  // ruleid: switch-case-constant-propagation-c
  exec(CMD);
}

// Bare case labels read enum values and must not inherit the scrutinee's
// taint through a synthetic pattern binding.
void caseLabelIsNotABindingTarget() {
  int y = taint_source();
  switch (y) {
    case RED:
      break;
    default:
      break;
  }

  // ok: taint-switch-case-c
  sink(RED);

  // Taint out of the scrutinee itself is unaffected.
  // ruleid: taint-switch-case-c
  sink(y);
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
      // ruleid: taint-switch-case-c
      sink(value);
      break;
  }
  // ok: taint-switch-case-c
  sink(FIRST);
}

void default_first(int k) {
  int x = taint_source();
  switch (k) {
    default:
      // ruleid: taint-switch-case-c
      sink(x);
      break;
    case 1:
      break;
  }
}

void default_shares_a_branch(int k) {
  int x = taint_source();
  switch (k) {
    case 1:
    default:
      // ruleid: taint-switch-case-c
      sink(x);
      break;
  }
}

void default_falls_through(int k) {
  int y = 0;
  switch (k) {
    default:
      y = taint_source();
    case 1:
      // ruleid: taint-switch-case-c
      sink(y);
      break;
  }
}

void case_breaks_before_default(int k) {
  int y = 0;
  switch (k) {
    case 1:
      y = taint_source();
      break;
    default:
      // ok: taint-switch-case-c
      sink(y);
  }
}
