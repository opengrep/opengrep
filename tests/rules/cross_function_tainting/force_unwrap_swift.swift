class R {
    var path: String
    var other: String

    init(path: String, other: String) {
        self.path = path
        self.other = other
    }
}

func element_of_array() {
    var infos = [R]()
    infos.append(R(path: source(), other: "x"))
    let e = infos.first!
    // ruleid: force_unwrap_swift
    sink(e.path)
    // ok: force_unwrap_swift
    sink(e.other)
}

func optional_value() {
    let r: R? = R(path: source(), other: "x")
    // ruleid: force_unwrap_swift
    sink(r!.path)
    // ok: force_unwrap_swift
    sink(r!.other)
}
