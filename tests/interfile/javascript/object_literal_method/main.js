class Service {
  tainted(y) {
    // ruleid: object-literal-method
    sink(y);
  }
  constant(y) {
    // ok: object-literal-method
    sink(y);
  }
}

function outer() {
  const value = source();
  const object = {
    run() {
      const service = new Service();
      service.constant("constant");
      return service.tainted(value);
    },
  };
  return object.run();
}
