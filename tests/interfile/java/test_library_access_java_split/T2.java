class T2 {
    static void handlerNonMapOpaqueTaintFlows() {
        MyMap m = new MyMap();
        m.stored = source();
        // ruleid: test-library-access-taint
        sink(m.get("body"));
    }
}
