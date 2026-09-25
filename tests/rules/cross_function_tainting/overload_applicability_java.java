// An overload applies to a call when the static type of each argument is the
// parameter's type or a subtype of it. The call reaches the overrides of the
// overloads that apply, and no other override.
interface Node {}

class A implements Node {
    void accept(ByThis v, String s) {
        v.visit(this, s);
    }
}

class B implements Node {}

class ByA {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}
}

class ByAPrinter extends ByA {
    void visit(Node n, String s) {
        // todook: overload_applicability_java
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload_applicability_java
        sink(s);
    }
}

class ByNode {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}
}

class ByNodePrinter extends ByNode {
    void visit(Node n, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }

    void visit(A a, String s) {
        // ok: overload_applicability_java
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload_applicability_java
        sink(s);
    }
}

class ByCast {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}
}

class ByCastPrinter extends ByCast {
    void visit(Node n, String s) {
        // todook: overload_applicability_java
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload_applicability_java
        sink(s);
    }
}

class ByExternal {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}
}

class ByExternalPrinter extends ByExternal {
    void visit(Node n, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }

    void visit(B b, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }
}

class ByThis {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}
}

class ByThisPrinter extends ByThis {
    void visit(Node n, String s) {
        // todook: overload_applicability_java
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload_applicability_java
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload_applicability_java
        sink(s);
    }
}

class Calls {
    void byThis(ByThis v, A a) {
        a.accept(v, source());
    }

    void byA(ByA v, A a) {
        v.visit(a, source());
    }

    void byNode(ByNode v, Node n) {
        v.visit(n, source());
    }

    void byCast(ByCast v, Node n) {
        v.visit((A) n, source());
    }

    void byExternal(ByExternal v, Unresolved u) {
        v.visit(u, source());
    }

    static String source() { return "tainted"; }

    static void sink(Object x) {}
}
