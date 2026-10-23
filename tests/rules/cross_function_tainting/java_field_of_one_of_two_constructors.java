// A method of the class reads the fields any constructor may have stored.

class Tainted {
    public String shop;

    public Tainted(String name) {
        this.shop = source();
    }

    public Tainted() {
        shop = "constant";
    }

    void get() {
        // ruleid: test-field-of-one-of-two-constructors
        sink(this.shop);
    }
}

class Clean {
    public String shop;

    public Clean(String name) {
        this.shop = "a";
    }

    public Clean() {
        shop = "b";
    }

    void get() {
        // ok: test-field-of-one-of-two-constructors
        sink(this.shop);
    }
}
