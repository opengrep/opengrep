const g = source();
const safe = "ok";

// ruleid: test-module-global-source-js
sink(g);

function main() {
  // ruleid: test-module-global-source-js
  sink(g);
}

function control() {
  // ok: test-module-global-source-js
  sink(safe);
}
