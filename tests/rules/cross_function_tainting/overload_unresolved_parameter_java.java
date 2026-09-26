import com.missing.Missing;
import com.missing.Base;

// The classes Missing and Base are not in the program. When every
// supertype of the argument's class is known, a parameter of class Missing
// cannot accept the argument, unless the language makes Missing a supertype
// of every class or of every class of the argument's kind.
interface Shape {}

class Leaf implements Shape {}

class Partial extends Base implements Shape {}

enum Colour { RED }

class Box<U> {
    void put(Missing m, String s) {
        // ok: overload_unresolved_parameter_java
        sink(s);
    }

    void put(U u, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    static void sink(String x) {}
}

class Overloads {
    void own(Missing m, String s) {
        // ok: overload_unresolved_parameter_java
        sink(s);
    }

    void own(Shape shape, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    void object(Missing m, String s) {
        // ok: overload_unresolved_parameter_java
        sink(s);
    }

    void object(Object o, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    void generic(Missing m, String s) {
        // ok: overload_unresolved_parameter_java
        sink(s);
    }

    <T> void generic(T t, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    void log(Missing m, String s) {
        // ok: overload_unresolved_parameter_java
        sink(s);
    }

    void log(Comparable c, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    void partial(Missing m, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    void partial(Shape shape, String s) {
        // ruleid: overload_unresolved_parameter_java
        sink(s);
    }

    static void sink(String x) {}
}

class Calls {
    void own(Overloads o, Leaf leaf) {
        o.own(leaf, source());
    }

    void object(Overloads o, Leaf leaf) {
        o.object(leaf, source());
    }

    void generic(Overloads o, Leaf leaf) {
        o.generic(leaf, source());
    }

    void log(Overloads o, Colour colour) {
        o.log(colour, source());
    }

    void partial(Overloads o, Partial partial) {
        o.partial(partial, source());
    }

    void boxed(Box<Leaf> box, Leaf leaf) {
        box.put(leaf, source());
    }

    static String source() { return "tainted"; }
}
