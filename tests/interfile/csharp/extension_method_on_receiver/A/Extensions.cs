namespace A {
    static class Extensions {
        public static void Handle(this Payload p, string x) {
            // ruleid: extension-method-on-receiver
            sink(x);
        }
    }
}
