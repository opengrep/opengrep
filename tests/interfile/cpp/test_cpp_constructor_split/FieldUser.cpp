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
