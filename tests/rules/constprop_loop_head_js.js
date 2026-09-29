function sameConstantInLoop() {
  let x = "a";
  while (c()) {
    x = "a";
  }
  // ruleid: constprop-loop-head-js
  sink(x);
}

function otherConstantInLoop() {
  let x = "a";
  while (c()) {
    x = "b";
  }
  // ok: constprop-loop-head-js
  sink(x);
}

function constantBeforeLoop() {
  let x = "a";
  while (c()) {
    f();
  }
  // ruleid: constprop-loop-head-js
  sink(x);
}
