mod m {
    fn f(c: String) {
        // ruleid: bare-call-in-module
        sink(c)
    }
    fn g() {
        f(source())
    }
}

mod n {
    fn f(c: String) {
        // ok: bare-call-in-module
        sink(c)
    }
}
