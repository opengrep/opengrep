// A lambda at an unreachable point reports nothing, as the code around it.
function f() {
  return;
  // ok: lambda-sig-js-unreachable
  setTimeout(() => sink(source()));
}

function g() {
  // ruleid: lambda-sig-js-unreachable
  setTimeout(() => sink(source()));
}
