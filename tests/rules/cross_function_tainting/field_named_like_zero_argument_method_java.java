import java.util.ArrayList;
import java.util.List;

class Holder {
    List<String> peek = new ArrayList<>();
}

class Test {
    void peekField() {
        Holder a = new Holder();
        a.peek.add(source());
        // ruleid: field-named-like-zero-argument-method
        sink(a.peek);
    }

    void peekFieldClean() {
        Holder a = new Holder();
        a.peek.add("safe");
        // ok: field-named-like-zero-argument-method
        sink(a.peek);
    }
}
