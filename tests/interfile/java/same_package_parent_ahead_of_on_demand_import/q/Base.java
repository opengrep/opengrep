package q;

public class Base {
    public void f(String x) {
        // ok: same-package-parent-ahead-of-on-demand-import
        sink(x);
    }
}
