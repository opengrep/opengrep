class FieldUser {
    public String name;

    public FieldUser() {
        this.name = "";
    }

    public String getProfile() {
        // ruleid: java_constructor_sqli
        String query = "SELECT * FROM users WHERE name = " + this.name;
        return query;
    }
}
