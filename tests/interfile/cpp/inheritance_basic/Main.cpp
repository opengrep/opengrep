#include <cstdlib>
#include <string>

std::string source() {
    const char *env = std::getenv("SECRET");
    return env ? std::string(env) : "";
}

int main() {
    std::string tainted = source();
    Dog d;
    d.process(tainted);
    return 0;
}
