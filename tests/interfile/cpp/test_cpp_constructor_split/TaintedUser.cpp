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
