#include <string>

void sink(const std::string &x);

// Two namespaces in ONE file.  `Package`/`PackageEnd` directives bracket
// each region.  The old first-`Package`-wins extraction attributed EVERY
// class in this file to the first namespace `a`, so `Base` (really in `b`)
// got the qn `a.Base`.  `struct Client : b::Base` in app.cpp then resolves
// its parent by the path `b.Base`, which never matches `a.Base` — no
// inheritance, no finding.  With per-region attribution `Base` is `b.Base`.
namespace a {
    struct Aux {
        void ping() {}
    };
}

namespace b {
    struct Base {
        void handle(const std::string &x) {
            // ruleid: multi-namespace-class
            sink(x);
        }
    };
}
