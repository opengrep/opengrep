function outerSafe([a]) {
  function innerSafe([b]) {
    // ok: nested_destructuring_function
    sink(b);
  }
  innerSafe(["safe"]);
}

function outerRelay([a]) {
  function innerRelay([b]) {
    // ruleid: nested_destructuring_function
    sink(b);
  }
  innerRelay([a]);
}

outerSafe([source()]);
outerRelay([source()]);
