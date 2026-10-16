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

void nestedConstantIsNotBound() {
  var k = taint_source();
  switch (k) {
    case [CMD]:
      // ok: taint-switch-case-dart
      sink(CMD);
      break;
  }
}

void nestedConstantIsCompared() {
  var x = taint_source();
  switch ([2]) {
    case [CMD]:
      // ok: taint-switch-case-dart
      sink(x);
      break;
  }
}

void logicalOrPattern() {
  var x = taint_source();
  switch (2) {
    case 1 || 3:
      // ok: taint-switch-case-dart
      sink(x);
      break;
  }
}

void restElementMatchesLongerList() {
  var x = taint_source();
  switch ([1, 2, 3]) {
    case [1, ...]:
      // ruleid: taint-switch-case-dart
      sink(x);
      break;
  }
}

void mapPatternMatchesBiggerMap() {
  var x = taint_source();
  switch ({'a': 1, 'b': 2}) {
    case {'a': 1}:
      // ruleid: taint-switch-case-dart
      sink(x);
      break;
  }
}

void emptyMapPatternMatchesAnyMap() {
  var x = taint_source();
  switch ({'a': 1}) {
    case {}:
      // ruleid: taint-switch-case-dart
      sink(x);
      break;
  }
}
