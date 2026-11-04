// A function read from a table of a few functions through an index that the
// analysis does not know is any of them: a call of it calls each.
function array(i) {
  const handlers = [(x) => "safe", (x) => {
    // ruleid: js_call_through_dynamic_index_calls_each_function
    sink(x);
  }];
  handlers[i](source());
}
function dict(k) {
  const handlers = { a: (x) => x, b: (x) => "safe" };
  // ruleid: js_call_through_dynamic_index_calls_each_function
  sink(handlers[k](source()));
  const clean = { a: (x) => "safe", b: (x) => "safe" };
  // ok: js_call_through_dynamic_index_calls_each_function
  sink(clean[k](source()));
}
