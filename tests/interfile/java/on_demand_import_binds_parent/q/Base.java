package q;

public class Base {
    public void f(String x) {
        // ruleid: on-demand-import-binds-parent
        sink(x);
    }
}
