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
