// ruleid: function-type-parameter-typescript-first
function f(cb: (err) => void) {}
// ok: function-type-parameter-typescript-first
function g(cb: (e) => void) {}
