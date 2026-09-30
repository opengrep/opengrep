struct Conv {
    Conv(int n) {}
};

struct Plain {};

struct Wrapped {
    Wrapped(Plain p) {}
};

struct Target {};

struct Holder {
    operator Target() const { return Target(); }
};

struct Strict {
    explicit Strict(int n) {}
};

char *from_int(Conv c) {
    return source();
}

char *from_class(Wrapped w) {
    return source();
}

char *by_conversion_function(Target t) {
    return source();
}

char *one_of_several(Conv c) {
    return source();
}

char *one_of_several(char *p) {
    return "";
}

char *explicit_only(Strict s) {
    return source();
}

char *explicit_only(long n) {
    return "";
}

char *standard_first(Conv c) {
    return source();
}

char *standard_first(long n) {
    return "";
}

char *boolean_conversion(bool b) {
    return source();
}

char *clean(Conv c) {
    return "";
}

void f() {
    Plain p;
    Holder h;
    // ruleid: user-defined-conversion
    sink(from_int(1));
    // ruleid: user-defined-conversion
    sink(from_class(p));
    // ruleid: user-defined-conversion
    sink(by_conversion_function(h));
    // ruleid: user-defined-conversion
    sink(one_of_several(1));
    // ok: user-defined-conversion
    sink(explicit_only(1));
    // ok: user-defined-conversion
    sink(standard_first(1));
    // ruleid: user-defined-conversion
    sink(boolean_conversion(1));
    // ok: user-defined-conversion
    sink(clean(1));
}
