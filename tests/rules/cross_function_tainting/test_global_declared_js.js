let G = "";
let C = "";

function writeTainted() {
  G = source();
}

const writeTaintedArrow = () => {
  G = source();
};

function writeClean() {
  C = "safe";
}

function readAfterWrite() {
  writeTainted();
  // ruleid: test-global-declared-js
  sink(G);
}

function readAfterArrowWrite() {
  writeTaintedArrow();
  // ruleid: test-global-declared-js
  sink(G);
}

function readClean() {
  writeClean();
  // ok: test-global-declared-js
  sink(C);
}

function readShadowed() {
  let G = "safe";
  writeTainted();
  // ok: test-global-declared-js
  sink(G);
}
