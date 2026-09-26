protocol P {
    func required(_ x: String)
}

extension P {
    func required(_ x: String) {}
    func extra(_ x: String) {}
}

class C: P {
    func required(_ x: String) {
        // ruleid: protocol-requirement-vs-extension
        sink(x)
    }

    func extra(_ x: String) {
        // ok: protocol-requirement-vs-extension
        sink(x)
    }
}

func run(p: P) {
    p.required(source())
    p.extra(source())
}
