// An array or an object built from the parameters and returned keeps its
// parts apart: the part that holds a clean argument carries no taint at the
// caller.
class Dto {
    String uid;
    String folder;
}

class Pair {
    String uid;
    String folder;

    Pair(String uid, String folder) {
        this.uid = uid;
        this.folder = folder;
    }
}

record Rec(String uid, String folder) {}

class CompositeReturnField {
    Pair made(String uid, String folder) {
        return new Pair(uid, folder);
    }

    Rec rec(String uid, String folder) {
        return new Rec(uid, folder);
    }

    void constructed() {
        Pair p = made(source(), "x");
        // ruleid: composite_return_field_java
        sink(p.uid);
        // ok: composite_return_field_java
        sink(p.folder);
    }

    void recorded() {
        Rec r = rec(source(), "x");
        // ruleid: composite_return_field_java
        sink(r.uid());
        // todook: composite_return_field_java
        sink(r.folder());
    }

    String[] pair(String uid, String folder) {
        return new String[] {uid, folder};
    }

    Dto dto(String uid, String folder) {
        Dto d = new Dto();
        d.uid = uid;
        d.folder = folder;
        return d;
    }

    void indexed() {
        String[] t = pair(source(), "x");
        // ruleid: composite_return_field_java
        sink(t[0]);
        // ok: composite_return_field_java
        sink(t[1]);
    }

    void fields() {
        Dto d = dto(source(), "x");
        // ruleid: composite_return_field_java
        sink(d.uid);
        // ok: composite_return_field_java
        sink(d.folder);
    }

    void local() {
        String[] t = new String[] {source(), "x"};
        // ruleid: composite_return_field_java
        sink(t[0]);
        // ok: composite_return_field_java
        sink(t[1]);
    }
}
