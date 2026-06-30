public class Test {
    void f(Object o) {
        // pattern matching for instanceof (javaext: 16): binds the variable
        // MATCH:
        if (o instanceof String s) {
            System.out.println(s);
        }
        // plain instanceof without a binding should NOT match the pattern
        if (o instanceof String) {
            return;
        }
        // MATCH:
        boolean b = o instanceof String str && str.length() > 0;
    }
}
