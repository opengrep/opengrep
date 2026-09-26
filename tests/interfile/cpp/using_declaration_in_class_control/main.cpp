struct B {
    void handle(int x) {
        // ok: using-declaration-in-class-control
        sink(x);
    }
};

struct D : B {
    void handle(const char *s) {}
};

void run(D d) {
    d.handle(source());
}
