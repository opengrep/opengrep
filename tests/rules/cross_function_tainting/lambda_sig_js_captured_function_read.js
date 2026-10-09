// A lambda reading a captured variable that holds a function reads the
// function, not what it returns; calling it, or a function held by a field
// of it, is a call.
function outer() {
  const f = () => source();
  const g = () => {
    // ok: lambda-sig-js-captured-function-read
    sink(f);
    // ruleid: lambda-sig-js-captured-function-read
    sink(f());
  };
  g();
}
outer();

function passes() {
  const f = () => source();
  const g = () => {
    exec(f);
  };
  g();
}
function exec(cb) {
  // ok: lambda-sig-js-captured-function-read
  sink(cb);
}
passes();

function field() {
  const utils = { read: () => source() };
  const g = () => {
    // ok: lambda-sig-js-captured-function-read
    sink(utils.read);
    // ruleid: lambda-sig-js-captured-function-read
    sink(utils.read());
  };
  g();
}
field();
