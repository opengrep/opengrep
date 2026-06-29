class C {
    void f(Outer outer) {
        // qualified instance creation x.new Inner(...)
        // MATCH:
        Object o = outer.new Inner(1, 2);
    }
}
