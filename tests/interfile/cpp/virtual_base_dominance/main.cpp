struct A {
    virtual void handle(int x) {
        // ok: virtual-base-dominance
        sink(x);
    }
};

struct L : public virtual A {};

struct R : public virtual A {
    void handle(int x) {
        // ruleid: virtual-base-dominance
        sink(x);
    }
};

struct D : L, R {};

void run(D *d) {
    d->handle(source());
}
