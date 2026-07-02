class IntermethodClass {
    public String taintMethod() {
        return source();
    }

    public String sinkMethod() {
        // ruleid: java_constructor_sqli
        String query = "SELECT * FROM users WHERE name = " + this.taintMethod();
        return query;
    }
}
