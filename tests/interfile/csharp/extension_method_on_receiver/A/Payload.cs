namespace A {
    class Payload {
        public void Keep(string x) {
            // ok: extension-method-on-receiver
            sink(x);
        }
    }
}
