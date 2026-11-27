package p;

class Base {
    void f(String x) {
        // ruleid: same-package-parent-ahead-of-on-demand-import
        sink(x);
    }
}
