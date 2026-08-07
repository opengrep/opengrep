var state = null;

function setup() {
  // A bare assignment in JS mutates the outer binding; it must resolve to
  // the module-level `state`, not declare a function-local.
  state = init();
}

function read() {
  return state;
}
