class Base {}
class Derived extends Base {}
class A {
  static void f(Base b) {
    // ok: java-overload-by-class-of-undeclared-variable
    sink(b);
  }

  static void f(Derived d) {
    // ruleid: java-overload-by-class-of-undeclared-variable
    sink(d);
  }

  static void run() {
    var d = new Derived();
    d.x = source();
    f(d);
  }
}
