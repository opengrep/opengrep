#include <iostream>
#include <string>

void sink(const std::string &x) {
    std::cout << x;
}

class Store {
    std::string data;
public:
    Store(const std::string &d) {
        this->data = d;
    }
    void send() {
        // ruleid: test-constructor-taint
        sink(this->data);
    }
};
