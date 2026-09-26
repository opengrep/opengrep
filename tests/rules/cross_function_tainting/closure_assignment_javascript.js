function one(uid, folder) {
  return { uid: uid, folder: folder };
}

function run(f) {
  f();
}

function scalarCalledByVariable() {
  let s = "";
  const f = () => { s = source(); };
  f();
  // ruleid: closure_assignment_javascript
  sink(s);
}

function scalarCalledImmediately() {
  let s = "";
  (() => { s = source(); })();
  // ruleid: closure_assignment_javascript
  sink(s);
}

function scalarPassedToCall() {
  let s = "";
  run(() => { s = source(); });
  // ruleid: closure_assignment_javascript
  sink(s);
}

function scalarReadAndWritten() {
  let s = "";
  const f = () => { console.log(s); s = source(); };
  f();
  // ruleid: closure_assignment_javascript
  sink(s);
}

function returnedCalledByVariable() {
  let e;
  const f = () => { e = one(source(), 3); };
  f();
  // ruleid: closure_assignment_javascript
  sink(e.uid);
  // ok: closure_assignment_javascript
  sink(e.folder);
}

function returnedReadAndWritten() {
  let e;
  const f = () => { console.log(e); e = one(source(), 3); };
  f();
  // ruleid: closure_assignment_javascript
  sink(e.uid);
  // ok: closure_assignment_javascript
  sink(e.folder);
}

function notCalled() {
  let s = "";
  const f = () => { s = source(); };
  // ok: closure_assignment_javascript
  sink(s);
}
