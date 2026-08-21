class Animal {
    void process(String data) {
        // ruleid: test-inheritance-basic
        sink(data);
    }

    static void sink(String x) {
        System.out.println(x);
    }
}
