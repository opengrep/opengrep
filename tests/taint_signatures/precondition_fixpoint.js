// Two functions calling each other, with a global reaching a sink that has a
// labelled requirement: each round of the fixpoint instantiates the other's
// previous signature, whose sink effect already carries the global's label,
// and the precondition must stay the same formula rather than gain a
// conjunction per round.
current = null;

function ping(n) {
  sink(current);
  if (n > 0) {
    pong(n - 1);
  }
}

function pong(n) {
  ping(n);
}

function go() {
  current = sourceA();
  ping(3);
}
