// A class inherits the default methods of the interfaces it implements, and
// a call on this inside a default method reaches the class's own method.
// A call on a value of the interface type reaches the implementing classes.
interface Handler {
    void m(String x);

    default void run(String x) {
        this.m(x);
    }
}
