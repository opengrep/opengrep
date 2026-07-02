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

        // Test chained method call: new Constructor(tainted).method()
        // ruleid: java_constructor_sqli
        String chainedResult = "SELECT * FROM users WHERE name = " + new User(source()).getProfile();

        return;
    }
}