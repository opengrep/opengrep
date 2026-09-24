#include <string>

void sink(const std::string &x);

class Animal {
public:
    void process(const std::string &data) {
        // ruleid: test-inheritance-basic
        sink(data);
    }
};
