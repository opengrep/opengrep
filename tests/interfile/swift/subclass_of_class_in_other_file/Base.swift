class Base {
    func f(_ x: String) {
        // ruleid: subclass-of-class-in-other-file
        sink(x)
    }
}
