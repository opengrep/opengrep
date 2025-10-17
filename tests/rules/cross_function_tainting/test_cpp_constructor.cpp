#include <string>

class TaintedUser {
private:
    std::string key;

public:
    TaintedUser(std::string seller) {
        this->key = source();
    }

    std::string props() {
        // ruleid: cpp_constructor_sqli
        return sink(this->key);
    }
};

class User {
private:
    std::string name;

public:
    User(std::string userName) {
        this->name = userName;
    }
    
    std::string getProfile() {
        // ruleid: cpp_constructor_sqli
        return sink(this->name);
    }
};

class FieldUser {
public:
    std::string name;

    FieldUser() {
        this->name = "";
    }
    
    std::string getProfile() {
        // ruleid: cpp_constructor_sqli
        return sink(this->name);
    }
};

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