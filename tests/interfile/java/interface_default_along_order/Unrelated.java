class Unrelated {
    public void m(String x) {
        // ok: interface_default_along_order
        sink(x);
    }

    public void run(String x) {
        // ok: interface_default_along_order
        sink(x);
    }
}
