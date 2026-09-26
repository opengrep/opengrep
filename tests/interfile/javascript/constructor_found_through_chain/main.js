class Base {
  constructor(x) {
    // ruleid: constructor-found-through-chain
    sink(x);
  }
}

class Middle extends Base {}

class Leaf extends Middle {}

function run() {
  new Leaf(source());
}
