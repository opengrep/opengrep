// A block body returns a value only through a return statement.
function ends_with_new() {
  const f = function (x) { new Foo(x); };
  // ok: js_block_closure_without_return
  sink(f(source()));
}

function returns_new() {
  const f = function (x) { return new Foo(x); };
  // ruleid: js_block_closure_without_return
  sink(f(source()));
}
