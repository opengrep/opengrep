int main() {
    std::string taintedInput = source();
    User user(taintedInput);
    std::string result = user.getProfile();
    
    // Test field assignment taint flow
    std::string taintedInput2 = source();
    FieldUser fieldUser;
    fieldUser.name = taintedInput2;
    std::string fieldResult = fieldUser.getProfile();
    
    return 0;
}