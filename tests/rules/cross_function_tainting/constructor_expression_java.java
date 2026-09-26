class Pair {
    String uid;
    String folder;

    Pair(String uid, String folder) {
        this.uid = uid;
        this.folder = folder;
    }
}

class ConstructorExpression {
    Pair made(String uid, String folder) {
        return new Pair(uid, folder);
    }

    Pair assigned(String uid, String folder) {
        Pair p;
        p = new Pair(uid, folder);
        return p;
    }

    Pair declared(String uid, String folder) {
        Pair p = new Pair(uid, folder);
        return p;
    }

    void read(Pair p) {
        // ok: constructor_expression_java
        sink(p.folder);
    }

    void returned() {
        Pair p = made(source(), "x");
        // ruleid: constructor_expression_java
        sink(p.uid);
        // ok: constructor_expression_java
        sink(p.folder);
    }

    void returnedAfterAssignment() {
        Pair p = assigned(source(), "x");
        // ruleid: constructor_expression_java
        sink(p.uid);
        // ok: constructor_expression_java
        sink(p.folder);
    }

    void returnedAfterDeclaration() {
        Pair p = declared(source(), "x");
        // ruleid: constructor_expression_java
        sink(p.uid);
        // ok: constructor_expression_java
        sink(p.folder);
    }

    void localAssignment() {
        Pair p;
        p = new Pair(source(), "x");
        // ruleid: constructor_expression_java
        sink(p.uid);
        // ok: constructor_expression_java
        sink(p.folder);
    }

    void localDeclaration() {
        Pair p = new Pair(source(), "x");
        // ruleid: constructor_expression_java
        sink(p.uid);
        // ok: constructor_expression_java
        sink(p.folder);
    }

    void argument() {
        read(new Pair(source(), "x"));
    }

    void chained() {
        // ok: constructor_expression_java
        sink(new Pair(source(), "x").folder);
    }
}
