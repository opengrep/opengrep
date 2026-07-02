class TaintedUser {
    private String key;

    public TaintedUser(String seller) {
        this.key = source();
    }

    public void props() {
        // ruleid: java_constructor_sqli
        String query = "SELECT * FROM table WHERE name = " + this.key;
        return;
    }
}
