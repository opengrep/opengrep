class Calls {
    void g(int n) {
    }

    void g(String s) {
    }

    void run() {
        g(1);
        g("constant");
    }
}
