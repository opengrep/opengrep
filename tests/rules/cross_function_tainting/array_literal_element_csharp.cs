class ArrayLiteralElement {
    string[] Pair(string uid, string folder) {
        return new string[] { uid, folder };
    }

    void LocalArray() {
        string[] t = new string[] { source(), "x" };
        // ruleid: array_literal_element_csharp
        sink(t[0]);
        // ok: array_literal_element_csharp
        sink(t[1]);
    }

    void ReturnedArray() {
        string[] t = Pair(source(), "x");
        // ruleid: array_literal_element_csharp
        sink(t[0]);
        // ok: array_literal_element_csharp
        sink(t[1]);
    }

    void AssignedArray() {
        string[] t;
        t = new string[] { source(), "x" };
        // ruleid: array_literal_element_csharp
        sink(t[0]);
        // ok: array_literal_element_csharp
        sink(t[1]);
    }
}
