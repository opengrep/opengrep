package com.visit;

import com.nodes.A;
import com.nodes.B;
import com.nodes.Node;

// The argument and parameter classes are imported from another package.
// An overload applies to a call when the static type of each argument is
// the parameter's type or a subtype of it.
class ByA {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}

    static void sink(Object x) {}
}

class ByAPrinter extends ByA {
    void visit(Node n, String s) {
        // ok: overload-applicability-imported
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload-applicability-imported
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload-applicability-imported
        sink(s);
    }
}

class ByNode {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}

    static void sink(Object x) {}
}

class ByNodePrinter extends ByNode {
    void visit(Node n, String s) {
        // ruleid: overload-applicability-imported
        sink(s);
    }

    void visit(A a, String s) {
        // ok: overload-applicability-imported
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload-applicability-imported
        sink(s);
    }
}

class ByCast {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}

    static void sink(Object x) {}
}

class ByCastPrinter extends ByCast {
    void visit(Node n, String s) {
        // ok: overload-applicability-imported
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload-applicability-imported
        sink(s);
    }

    void visit(B b, String s) {
        // ok: overload-applicability-imported
        sink(s);
    }
}

class ByExternal {
    void visit(Node n, String s) {}
    void visit(A a, String s) {}
    void visit(B b, String s) {}

    static void sink(Object x) {}
}

class ByExternalPrinter extends ByExternal {
    void visit(Node n, String s) {
        // ruleid: overload-applicability-imported
        sink(s);
    }

    void visit(A a, String s) {
        // ruleid: overload-applicability-imported
        sink(s);
    }

    void visit(B b, String s) {
        // ruleid: overload-applicability-imported
        sink(s);
    }
}

class Calls {
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

    static String source() {
        return "tainted";
    }
}
