public class TaintedUser {
    private String key;

    public TaintedUser(String seller) {
        this.key = source();
    }

    public void props() {
        // ruleid: test
        sink(this.key);
    }
}

public class User {
    private String name;

    public User(String userName) {
        this.name = userName;
    }
    
    public String getProfile() {
    // ruleid: test
        sink(this.name);
        return this.name;
    }
}

public class FieldUser {
    public String name;

    public FieldUser() {
        this.name = '';
    }
    
    public String getProfile() {
    // ruleid: test
        sink(this.name);
        return this.name;
    }
}

public class Main {
    public static void main() {
        String taintedInput = source();
        User user = new User(taintedInput);
        String result = user.getProfile();
        
        // Test field assignment taint flow
        String taintedInput2 = source();
        FieldUser fieldUser = new FieldUser();
        fieldUser.name = taintedInput2;
        String fieldResult = fieldUser.getProfile();
    }
}
