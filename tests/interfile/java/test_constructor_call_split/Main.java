class Main {
    public static void main(String[] args) {
        String taintedInput = source();
        User user = new User(taintedInput);
        String result = user.getProfile();
    }
}
