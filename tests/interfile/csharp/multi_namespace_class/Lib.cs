// A C# namespace block delimits a package region, so the class `Base` in the
// second region of this file carries the qualified name `B.Base` that
// `Client : B.Base` in App.cs resolves against.
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
