public virtual class Base {
    public void f(String x) {
        // ruleid: extends-class-in-other-file
        sink(x);
    }
}
