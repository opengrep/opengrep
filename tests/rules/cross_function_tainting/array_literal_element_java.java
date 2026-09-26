class ArrayLiteralElement {
    String[] pair(String uid, String folder) {
        return new String[] {uid, folder};
    }

    void localArray() {
        String[] t = new String[] {source(), "x"};
        // ruleid: array_literal_element_java
        sink(t[0]);
        // ok: array_literal_element_java
        sink(t[1]);
    }

    void returnedArray() {
        String[] t = pair(source(), "x");
        // ruleid: array_literal_element_java
        sink(t[0]);
        // ok: array_literal_element_java
        sink(t[1]);
    }

    void shortArray() {
        String[] t = {source(), "x"};
        // ruleid: array_literal_element_java
        sink(t[0]);
        // ok: array_literal_element_java
        sink(t[1]);
    }

    void assignedArray() {
        String[] t;
        t = new String[] {source(), "x"};
        // ruleid: array_literal_element_java
        sink(t[0]);
        // ok: array_literal_element_java
        sink(t[1]);
    }
}
