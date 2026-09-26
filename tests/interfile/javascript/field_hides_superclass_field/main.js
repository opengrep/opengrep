class Base {
  constructor() {
    this.handler = function (x) {
      // ok: field-hides-superclass-field
      sink(x);
    };
  }
}

class Sub extends Base {
  constructor() {
    super();
    this.handler = function (x) {
      // ruleid: field-hides-superclass-field
      sink(x);
    };
  }
}

function run() {
  new Sub().handler(source());
}
