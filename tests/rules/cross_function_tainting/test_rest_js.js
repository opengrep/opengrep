// JS/TS rest-binding destructure [a, b, ...rest]: the rest binding
// receives positions from k onwards of the incoming list/array. A source
// at any position >= k reaches the rest binding; a source at a fixed
// slot before ...rest does not.

// Body destructure forms

function bodyHandlerHead(arr) {
  const [head, ...rest] = arr;
  // ruleid: test-rest-js
  sink(head);
}

function bodyCallerHead() {
  bodyHandlerHead([source(), "ok"]);
}


function bodyHandlerRest(arr) {
  const [head, ...rest] = arr;
  // ruleid: test-rest-js
  sink(rest);
}

function bodyCallerRest() {
  bodyHandlerRest(["safe", source()]);
}


function bodyHandlerRestDeep(arr) {
  const [head, ...rest] = arr;
  // ruleid: test-rest-js
  sink(rest);
}

function bodyCallerRestDeep() {
  bodyHandlerRestDeep(["safe", "a", "b", "c", source()]);
}


function bodyHandlerCleanHead(arr) {
  const [head, ...rest] = arr;
  // ok: test-rest-js
  sink(head);
}

function bodyCallerCleanHead() {
  bodyHandlerCleanHead(["safe", "ok"]);
}


function bodyHandlerRestSourceInHead(arr) {
  const [head, ...rest] = arr;
  // ok: test-rest-js
  sink(rest);
}

function bodyCallerRestSourceInHead() {
  bodyHandlerRestSourceInHead([source(), "ok"]);
}


// Two fixed slots before rest: [a, b, ...rest]; rest covers [2..]

function bodyHandlerRestPos2(arr) {
  const [a, b, ...rest] = arr;
  // ruleid: test-rest-js
  sink(rest);
}

function bodyCallerRestPos2() {
  bodyHandlerRestPos2(["safe", "ok", source()]);
}


function bodyHandlerRestSourceInB(arr) {
  const [a, b, ...rest] = arr;
  // ok: test-rest-js
  sink(rest);
}

function bodyCallerRestSourceInB() {
  // source at position 1 binds b; rest covers [2..]
  bodyHandlerRestSourceInB(["safe", source(), "ok"]);
}


// Param destructure forms

function paramHandlerRest([head, ...rest]) {
  // ruleid: test-rest-js
  sink(rest);
}

function paramCallerRest() {
  paramHandlerRest(["safe", source()]);
}


function paramHandlerHead([head, ...rest]) {
  // ruleid: test-rest-js
  sink(head);
}

function paramCallerHead() {
  paramHandlerHead([source(), "ok"]);
}


function paramHandlerRestSourceInHead([head, ...rest]) {
  // ok: test-rest-js
  sink(rest);
}

function paramCallerRestSourceInHead() {
  paramHandlerRestSourceInHead([source(), "ok"]);
}


function paramHandlerCleanHead([head, ...rest]) {
  // ok: test-rest-js
  sink(head);
}

function paramCallerCleanHead() {
  paramHandlerCleanHead(["safe", "ok"]);
}
