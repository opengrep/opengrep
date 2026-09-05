// C# `namespace {}` blocks parse to `ModuleDef` (like TS), so they ride the
// Step-1 namespace-scope handling.  `Base` lives in the SECOND namespace; if
// the namespace scope is dropped it collapses to a bare `Base` and
// `Client : B.Base` in App.cs cannot resolve it.
namespace A {
    class Aux {
        void ping() {}
    }
}

namespace B {
    class Base {
        void handle(string x) {
            // ruleid: multi-namespace-class
            sink(x);
        }
    }
}
