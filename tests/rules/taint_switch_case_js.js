const CMD = 7;
const COMMAND = "rm";

function test(mode, commandMode) {
  var q = "";
  // ruleid: switch-case-constant-propagation-js
  exec(CMD);
  switch (mode) {
    case CMD:
      // ruleid: switch-case-constant-propagation-js
      exec(CMD);
      break;
    case B:
      q = taint_source();
      // ruleid: taint-switch-case-js
      sink(q);
      break;
  }
  // ruleid: switch-case-constant-propagation-js
  exec(CMD);
  switch (commandMode) {
    case COMMAND:
      // ruleid: switch-case-command-constant-js
      exec(COMMAND);
      break;
  }
}

function explicitShadowing(mode, CMD) {
  switch (mode) {
    case CMD:
      // ok: switch-case-constant-propagation-js
      exec(CMD);
  }
}

// Qualified and bare case labels read values; neither may inherit the
// scrutinee's taint through a synthetic pattern binding.
function caseLabelIsNotABindingTarget() {
  var x = taint_source();
  switch (x) {
    case Colors.RED:
      break;
    default:
      break;
  }

  // ok: taint-switch-case-js
  sink(Colors.RED);

  // ok: taint-switch-case-js
  sink(Colors);
}

// Taint out of the scrutinee itself is unaffected.
function scrutineeStillFlows() {
  var y = taint_source();
  switch (y) {
    case 1:
      // ruleid: taint-switch-case-js
      sink(y);
      break;
  }
}

function bareNameCaseIsNotABindingTarget() {
  var y = taint_source();
  switch (y) {
    case RED:
      break;
  }
  // ok: taint-switch-case-js
  sink(RED);
}

function caseReadsExistingTaint(mode) {
  const value = taint_source();
  switch (mode) {
    case value:
      // ruleid: taint-switch-case-js
      sink(value);
      break;
  }
  // ruleid: taint-switch-case-js
  sink(value);
}
