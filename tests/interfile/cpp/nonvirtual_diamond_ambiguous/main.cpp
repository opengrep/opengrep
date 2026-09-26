struct A {
    virtual void handle(int x) {
        // ok: nonvirtual-diamond-ambiguous
        sink(x);
    }
};

struct L : public A {};

struct R : public A {
    void handle(int x) {
        // ok: nonvirtual-diamond-ambiguous
        sink(x);
    }
};

struct D : L, R {};

void run(D *d) {
    d->handle(source());
}
