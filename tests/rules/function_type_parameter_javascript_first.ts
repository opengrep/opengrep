// ruleid: function-type-parameter-javascript-first
function f(cb: (err) => void) {}
// ok: function-type-parameter-javascript-first
function g(cb: (e) => void) {}
