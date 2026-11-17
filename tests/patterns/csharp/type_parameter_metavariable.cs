class A {
    // ERROR: match
    T Id<T>(T x) {
        return x;
    }

    int Size<T>(T x) {
        return 0;
    }
}
