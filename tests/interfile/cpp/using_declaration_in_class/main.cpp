struct B {
    void handle(int x) {
        // ruleid: using-declaration-in-class
        sink(x);
    }
};

struct D : B {
    using B::handle;
    void handle(const char *s) {}
};

void run(D d) {
    d.handle(source());
}
