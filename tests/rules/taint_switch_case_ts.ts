const CMD = 7;
const COMMAND = "rm";

function test(mode: number, commandMode: string) {
  var q = "";
  // ruleid: switch-case-constant-propagation-ts
  exec(CMD);
  switch (mode) {
    case CMD:
      // ruleid: switch-case-constant-propagation-ts
      exec(CMD);
      break;
    case B:
      q = taint_source();
      // ruleid: taint-switch-case-ts
      sink(q);
      break;
  }
  // ruleid: switch-case-constant-propagation-ts
  exec(CMD);
  switch (commandMode) {
    case COMMAND:
      // ruleid: switch-case-command-constant-ts
      exec(COMMAND);
      break;
  }
}

function explicitShadowing(mode: number, CMD: number) {
  switch (mode) {
    case CMD:
      // ok: switch-case-constant-propagation-ts
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

  // ok: taint-switch-case-ts
  sink(Colors.RED);

  // ok: taint-switch-case-ts
  sink(Colors);
}

// Taint out of the scrutinee itself is unaffected.
function scrutineeStillFlows() {
  var y = taint_source();
  switch (y) {
    case 1:
      // ruleid: taint-switch-case-ts
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
  // ok: taint-switch-case-ts
  sink(RED);
}
