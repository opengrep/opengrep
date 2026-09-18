namespace A {
    class Other {
        public void Handle(string x) {
            // ok: extension-method-on-receiver
            sink(x);
        }
    }
}
