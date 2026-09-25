class Main {
    static void main() {
        Impl impl = new Impl();
        impl.run(source());
    }

    static void use(Handler h) {
        h.m(source());
    }
}
