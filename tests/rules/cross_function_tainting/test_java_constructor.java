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

class User {
    private String name;

    public User(String userName) {
        this.name = userName;
    }
    
    public String getProfile() {
        // ruleid: java_constructor_sqli
        String query = "SELECT * FROM users WHERE name = " + this.name;
        return query;
    }
}

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

public class Main {
    public static void main(String[] args) {
        String taintedInput = source();
        User user = new User(taintedInput);
        String result = user.getProfile();

        // Test field assignment taint flow
        String taintedInput2 = source();
        FieldUser fieldUser = new FieldUser();
        fieldUser.name = taintedInput2;
        String fieldResult = fieldUser.getProfile();

        // Test intermethod taint flow
        IntermethodClass intermethodObj = new IntermethodClass();
        String intermethodResult = intermethodObj.sinkMethod();

        return;
    }
}