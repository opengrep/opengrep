class User {
    private String name;
    
    public User(String userName) {
        this.name = userName;
    }
    
    public String getProfile() {
        // ruleid:java_constructor_sqli
        String query = "SELECT * FROM users WHERE name = " + this.name;
        return query;
    }
}
