// A name bound to two lambdas holds either of them at the call.
let h;
if (cond) { h = (x) => "safe"; } else { h = (x) => x; }
// ruleid: js_name_bound_to_two_lambdas
sink(h(source()));
