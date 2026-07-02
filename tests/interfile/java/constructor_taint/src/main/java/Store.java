class Store {
    private String data;

    Store(String data) {
        this.data = data;
    }

    void send() {
        // ruleid: test-constructor-taint
        sink(this.data);
    }

    static void sink(String x) {
        System.out.println(x);
    }
}
