// A call through a receiver of static type Base selects an overload that
// Base declares, then reaches the overrides of that overload. An overload
// that only a subclass declares is not reachable from the call.
class A {}

class B {}

class Base {
    void visit(A a, String s) {}
}

class Sub extends Base {
    void visit(A a, String s) {
        // ruleid: dispatch_after_overload_java
        sink(s);
    }

    void visit(B b, String s) {
        // ok: dispatch_after_overload_java
        sink(s);
    }

    static void sink(Object x) {}
}

class Calls {
    void call(Base base, Unresolved u) {
        base.visit(u, source());
    }

    static String source() { return "tainted"; }
}
