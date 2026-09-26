class Visitor {
    //ERROR: match
    <T> void visit(T node) {}

    void plain(Object node) {}

    <U> void other(U node) {}

    //ERROR: match
    <T> T identity(T node) {
        return node;
    }
}
