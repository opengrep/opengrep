// A call selects the overload whose parameter types match the static types
// of its arguments, whatever the order of the declarations.
class Calls {
    void f(int n) {
        // ok: overload_selection_java
        sink(n);
    }

    void f(String s) {
        // ruleid: overload_selection_java
        sink(s);
    }

    void g(int n) {
        // ruleid: overload_selection_java
        sink(n);
    }

    void g(String s) {
        // ok: overload_selection_java
        sink(s);
    }

    void run(int clean) {
        f(clean);
        f(source());
        int tainted = source().length();
        g(tainted);
        g("constant");
    }

    static String source() { return "tainted"; }

    static void sink(Object x) {}
}
