struct B {
    void handle(int x) {}
};

//ERROR:
struct D : B {
    using B::handle;
    void handle(const char *s) {}
};

struct E : B {
    void handle(const char *s) {}
};
