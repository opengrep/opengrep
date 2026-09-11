namespace A {
    namespace B {
        class Util {
            public static void Run(string x) {
                // ruleid: nested-namespace-binds-type
                sink(x);
            }
        }
    }
}
