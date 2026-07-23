#include <string>

std::string source();

// Inherits the sink method from `b::Base` (declared in the SECOND namespace
// of lib.cpp).  The parent path `b.Base` only resolves if `Base` is
// attributed to namespace `b`, not the file's first namespace `a`.
struct Client : b::Base {};

int main() {
    std::string tainted = source();
    Client c;
    c.handle(tainted);
    return 0;
}
