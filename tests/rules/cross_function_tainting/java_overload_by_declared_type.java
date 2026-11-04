class Base {}
class Derived extends Base {}
class A {
  static void f(Base b) {
    // ruleid: java-overload-by-declared-type
    sink(b);
  }

  static void f(Derived d) {
    // ok: java-overload-by-declared-type
    sink(d);
  }

  static void run() {
    Base b = new Derived();
    b.x = source();
    f(b);
  }
}
