const CMD = 7;
const B = 2;
const RED = 3;

void test(int mode) {
  var q = "safe";
  // ruleid: switch-case-constant-propagation-dart
  exec(CMD);
  switch (mode) {
    case CMD:
      // ruleid: switch-case-constant-propagation-dart
      exec(CMD);
      break;
    case B:
      q = taint_source();
      // ruleid: taint-switch-case-dart
      sink(q);
      break;
  }
  // ruleid: switch-case-constant-propagation-dart
  exec(CMD);
}

void labelIsNotBound() {
  var y = taint_source();
  switch (y) {
    case RED:
      break;
  }
  // ok: taint-switch-case-dart
  sink(RED);
  // ruleid: taint-switch-case-dart
  sink(y);
}
