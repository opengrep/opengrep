// The static type of a cast to Node<?> is the class Node: the overload that
// takes Node<?> applies, the overload that takes A, a class that implements
// Node, does not.
interface Node<T> {}

class A implements Node<String> {}

class Visitor {
    void visit(Node<?> n, String s) {}
    void visit(A a, String s) {}
}

class Printer extends Visitor {
    void visit(Node<?> n, String s) {
        // ruleid: overload_parameterised_argument_java
        sink(s);
    }

    void visit(A a, String s) {
        // ok: overload_parameterised_argument_java
        sink(s);
    }

    static void sink(Object x) {}
}

class Calls {
    void call(Printer printer, Object o) {
        printer.visit((Node<?>) o, source());
    }

    static String source() { return "tainted"; }
}
