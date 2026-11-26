package r;

public class Base {
    public void f(String x) {
        // ok: on-demand-import-binds-parent
        sink(x);
    }
}
