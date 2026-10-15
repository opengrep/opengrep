// A [var] variable of a [for] initialiser, which naming does not resolve,
// read by a lambda: the lambda captures it.
function withVar() {
  for (var i = source(); i; ) {
    const g = () => {
      // ruleid: js_lambda_reads_unresolved_variable
      sink(i);
    };
    g();
  }
}

// The same with [let].
function withLet() {
  for (let j = source(); j; ) {
    const g = () => {
      // ruleid: js_lambda_reads_unresolved_variable
      sink(j);
    };
    g();
  }
}

function untainted() {
  for (let k = clean(); k; ) {
    const g = () => {
      // ok: js_lambda_reads_unresolved_variable
      sink(k);
    };
    g();
  }
}
