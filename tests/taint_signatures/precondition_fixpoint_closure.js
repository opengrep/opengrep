// A closure reaching a sink with a global, inside functions calling each
// other: the closure's signature keeps the global as a placeholder, each round
// instantiates the other function's previous signature, and the precondition
// must stay the same formula rather than gain a conjunction per round.
current = null;

function ping(n) {
  const next = () => {
    sink(current);
    if (n > 0) {
      pong(n - 1);
    }
  };
  next();
}

function pong(n) {
  ping(n);
}

function go() {
  current = sourceA();
  ping(3);
}
