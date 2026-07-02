#include <cstdlib>
#include <string>
#include <iostream>

std::string source() {
    const char *env = std::getenv("SECRET");
    return env ? std::string(env) : "";
}

class Store {
    std::string data;
public:
    Store(const std::string &d);
    void send();
};

int main() {
    std::string tainted = source();
    Store s(tainted);
    s.send();
    return 0;
}
