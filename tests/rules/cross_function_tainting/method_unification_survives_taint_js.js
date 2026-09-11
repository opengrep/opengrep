class A { run() {} }

class B { run() {} }

function go() {
  const a = new A();
  const b = new B();
  // ruleid: method_unification_survives_taint
  a.run();
  b.run();
  const x = source();
  // ruleid: method_unification_survives_taint_sink
  sink(x);
}
